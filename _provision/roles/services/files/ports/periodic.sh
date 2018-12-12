#!/bin/sh

_ports_base="https://svn.freebsd.org/ports/branches/"
_ports_branch=$(svnlite ls $_ports_base | awk '/^2.*Q./ { c = $0 } END { gsub("/$", "", c); print c }')
_ports_url="$_ports_base$_ports_branch"

if [ ! -d /usr/ports/.svn ]; then
    echo 'FreeBSD ports tree is not initialized.'
    echo 'Exiting...'
    exit 1
fi

_ports_cur=$(svnlite info /usr/ports |awk '/^URL:/ { print $2 }')

if [ "$_ports_cur" = "$_ports_url" ]; then
    svnlite update /usr/ports
else
    echo "Switching ports tree to $_ports_branch..."
    svnlite switch "$_ports_url" /usr/ports
fi
