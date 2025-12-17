#!/bin/bash

# This script is modified from https://askubuntu.com/a/1315976

# Run this script without any param for a dry run.
# Run the script as root and with exec param for removing old kernels after checking
# the list printed in the dry run.

uname -a
IN_USE=$(uname -a | awk '{ print $3 }' | sed 's/-generic//')
echo "Your in use kernel is $IN_USE"

OLD_KERNELS=$(
    dpkg --list |
        grep -v "$IN_USE" |
        grep -v "linux-image-generic" |
        grep -v "linux-headers-generic" |
        grep -Ei 'linux-image|linux-headers|linux-modules' |
        awk '{ print $2 }'
)
echo "Old Kernels to be removed:"
echo "$OLD_KERNELS"

if [ "$1" == "exec" ]; then
    yes | apt purge $(echo "$OLD_KERNELS" | tr '\n' ' ')
else
    echo "If all looks good, run it again like this: sudo remove_old_kernels.sh exec"
fi
