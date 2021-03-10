#!/bin/bash

BASEDIR=$(dirname $(readlink -f $0))
FCHOME=$(dirname $BASEDIR)

if [[ ! $(grep FCHOME ~/.bashrc) ]]; then
    echo -e "\nexport FCHOME=$FCHOME;. ${FCHOME}/bin/FC\n" >>~/.bashrc
fi
