#!/usr/bin/env bash

source $FCHOME/bash/lib.sh

function generate {
    echo "-IWall"
    ff -nc \\.h$ | xargs dirname | sort -u | sed s/^/-I/
}

proj-chtop

if [[ -f compile_flags.txt ]]; then
    fc-user-confirm "compile_flags.txt already exists, still continue?" || exit 0
fi

echo "Generate ..."
generate > compile_flags.txt
echo "Done"
