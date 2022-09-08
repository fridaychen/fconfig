#!/bin/bash

BASEDIR=$(dirname $(readlink -f $0))
FCHOME=$(readlink -f "$BASEDIR/../../")

case $(uname) in
    Darwin)
        RC=~/.profile
        ;;
    Linux)
        RC=~/.bashrc
        ;;
    *)
        echo "Unknown OS"
        exit
        ;;
esac

if [[ ! $(grep FCHOME "$RC") ]]; then
    echo -e "\nexport FCHOME=$FCHOME;source ${FCHOME}/bin/FC\n" >>$RC
fi
