# EC2 environments
if [[ -e $HOME/.ec2 ]]; then
    export EC2_PRIVATE_KEY="$(/bin/ls $HOME/.ec2/pk-*.pem)"
    export EC2_CERT="$(/bin/ls $HOME/.ec2/cert-*.pem)"
    export EC2_HOME="$(brew --prefix ec2-api-tools)/jars"
    export EC2_URL="https://ap-southeast-1.ec2.amazonaws.com"
    export EC2_KEYPAIR="$(/bin/ls $HOME/.ec2/ec2-*.pem)"
fi

# Instance configurations
export EC2_TYPE="t1.micro"
export EC2_KEYNAME="ec2-keypair"
export EC2_AMI="ami-7a057f28" # Ubuntu Server
export EC2_USERNAME="ubuntu"

# Helper functions
function ec2-instances {
    # Display a simplified ec2 instance information.
    ec2-describe-instances|grep INSTANCE|awk '{print $2 " " $4}'
}

function ec2-boot {
    # Startup ec2 instance.
    ec2-run-instances -t $EC2_TYPE -k $EC2_KEYNAME $EC2_AMI > /dev/null
    print "Waiting for EC2 instance to boot..."
    sleep 15
    ec2-instances
    print "To terminate, use: ec2-terminate-instances <instance_id>"
    print "If public address is not shown, run: ec2-instances"
}

function ec2-ssh {
    # SSH into Ubuntu ec2 server and open a SOCK proxy at port 9050.
    if [[ -n $1 ]]; then
        ssh -D 9050 -i $EC2_KEYPAIR $EC2_USERNAME@$1
    fi
}

function ec2-install-vpn {
    # SSH into Ubuntu ec2 and install PPTP server
    if [[ -n $1 ]]; then
        local installer="https://raw.github.com/gist/1156378/ffe06a792c86b771805ca0e6aad16823524ac9c9/pptpd.sh"
        ssh -i $EC2_KEYPAIR $EC2_USERNAME@$1 "sudo bash < <(curl -s $installer)"
    fi
}
