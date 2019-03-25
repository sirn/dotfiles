#!/bin/sh
#
# This is script for managing stageroot for entering GELI password over SSH.
#
# I'm currently using a FreeBSD server with full disk encryption as a remote
# workspace. However, full disk encryption with GELI on FreeBSD means I will
# have to enter the password to decrypt the disk during stage 1 boot. This is
# problematic for a remote server, since I cannot access console without using
# IPMI or KVM tools provided by the provider.
#
# Luckily, FreeBSD allows changing the root partition and restart the
# userspace without rebooting the kernel! This is done via changing the
# `vfs.root.mountfrom` kenv and run `reboot -r`. The idea here is to allocate
# few space off the disk as a stage 1 root partition to allow the machine to
# boot without decrypting the rest. Stage 1 root should run SSH with a script
# that can be used to decrypt the actual root and restart into it.
#
# This script does exactly that.
#

ZPOOL=zboot
MOUNTPOINT=/tmp/$ZPOOL
WORKDIR=$MOUNTPOINT/bootpool/stageroot

PRUNELIST_URL=https://raw.githubusercontent.com/mmatuska/mfsbsd/07cc1857fa1f17805d5e3ceaba210015484e962e/tools/prunelist
PRUNELIST_SHA256=d282a3c6a1ea52c4353c4f3621a806a5974c51d6ac6573ea546b21d68cbeec3d

if [ "$(uname)" != "FreeBSD" ]; then
    echo "Oink?"
    exit 1
fi

if [ ! -f /usr/src/Makefile ]; then
    echo "The world must be built."
    exit 1
fi

if [ "$(id -u)" != "0" ] || [ -z "$SUDO_USER" ]; then
    echo "Must be run with sudo."
    exit 1
fi

set -xe


## Setup ZFS
##

mkdir -p $MOUNTPOINT
zpool export $ZPOOL || true
zpool import -R $MOUNTPOINT zboot
zfs destroy $ZPOOL/stageroot || true
zfs create $ZPOOL/stageroot
zfs set compression=lz4 $ZPOOL/stageroot


## Install world
##

cd /usr/src || exit 1
make installworld distribution \
     DESTDIR=$WORKDIR \
     NO_FSCHG=1 \
     WITHOUT_CLANG=1 \
     WITHOUT_DICT=1 \
     WITHOUT_GAMES=1 \
     WITHOUT_LIB32=1 \
     WITHOUT_TOOLCHAIN=1


## Configure system
##

touch $WORKDIR/etc/rc.conf
touch $WORKDIR/etc/fstab

for KEY in hostname ifconfig_vtnet0 defaultrouter; do
    sysrc -R $WORKDIR $KEY="$(sysrc -n "$KEY")"
done

sysrc -R $WORKDIR sshd_enable=YES
sysrc -R $WORKDIR cron_enable=NO
sysrc -R $WORKDIR syslogd_enable=NO
sysrc -R $WORKDIR sendmail_enable=NONE
sysrc -R $WORKDIR sendmail_submit_enable=NO
sysrc -R $WORKDIR sendmail_outbound_enable=NO
sysrc -R $WORKDIR sendmail_msp_queue_enable=NO

for key_type in ecdsa ed25519 rsa; do
    for key in ssh_host_${key_type}_key ssh_host_${key_type}_key.pub; do
        cp /etc/ssh/$key $WORKDIR/etc/ssh/
    done
done

cat <<EOF > $WORKDIR/etc/motd
Welcome to restricted FreeBSD!
To continue, run "sudo unlock_boot".
EOF


## Setup user
##

chroot $WORKDIR pw user add "$SUDO_USER" -G wheel -m
mkdir -p "$WORKDIR/home/$SUDO_USER/.ssh/"
cp "/home/$SUDO_USER/.ssh/authorized_keys" "$WORKDIR/home/$SUDO_USER/.ssh/"
chown 1001:1001 "$WORKDIR/home/$SUDO_USER/.ssh/authorized_keys"


## Setup packages
##

chroot $WORKDIR env ASSUME_ALWAYS_YES=yes pkg bootstrap
pkg-static -c $WORKDIR install -y sudo ca_root_nss

cat <<EOF > $WORKDIR/usr/local/etc/sudoers.d/wheel
%wheel ALL=(ALL) NOPASSWD: ALL
EOF


## Setup unlock_boot script
##

cat <<EOF | sudo tee $WORKDIR/usr/local/bin/unlock_boot
#!/bin/sh
PASSWORD=""

get_pass() {
    stty -echo
    printf "%s: " "\$@"
    read -r PASSWORD
    stty echo
    printf "\\n"
}

if [ "\$(id -u)" != "0" ]; then
    echo "Must be run as root."
    exit 1
fi

zfs mount zboot
echo "Root partition need to be unlocked to continue."
get_pass "Enter GELI password"

for SPEC in root1.key:gpt/root1 swap1.key:gpt/swap1; do
    KEY="/bootpool/boot/keys/\${SPEC%%:*}"
    DISK="/dev/\${SPEC##*:}"
    if ! echo "\$PASSWORD" | geli attach -j - -k "\$KEY" "\$DISK"; then
        echo "Could not unlock \$DISK."
        exit 1
    fi
done

echo "Unlocked! You will be forced to relogin."
kenv vfs.root.mountfrom="zfs:zroot/ROOT/default"
reboot -r
EOF

chmod +x $WORKDIR/usr/local/bin/unlock_boot


## Cleanup
##

fetch -o /tmp/prunelist $PRUNELIST_URL
echo "$PRUNELIST_SHA256  /tmp/prunelist" | shasum -c -
while read -r f; do
    rm -rf "${WORKDIR:?}/$f"
done < /tmp/prunelist
rm /tmp/prunelist

rm -rf ${WORKDIR:?}/boot
mkdir -p $WORKDIR/bootpool

zfs set mountpoint=none $ZPOOL/stageroot
zpool export zboot
zpool import zboot
