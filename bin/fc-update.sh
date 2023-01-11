#!/usr/bin/env bash

source $FCHOME/bash/lib.sh

fc-enable-alias

# update system packages
fj --up

# update emacs
if [[ -d ~/opensource/emacs ]]; then
    cd ~/opensource/emacs/
    fc-user-confirm 'Update Emacs' && fc-compile-emacs.sh -f
fi

# update essential aur
if [[ -d ~/opensource/aur-essential ]]; then
    cd ~/opensource/aur-essential

    for x in *; do
        if [[ ! -d $x ]]; then
            continue
        fi

        echo "Checking AUR ${x} ..."

        cd $x
        gb
        cd ..
    done
fi
