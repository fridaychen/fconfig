#!/usr/bin/env bash

source $FCHOME/bash/lib.sh

TO_FMT=""

function arg-set {
    case $1 in
        t)
            TO_FMT=$2
            ;;
    esac
}

USAGE="Usage: audio-convert.sh [OPTION] [FILE]\n\n
  -t to format
"
ARGUMENTS="ht:"
source $FCHOME/bash/lib/argparser.sh

if [[ -z $TO_FMT ]]; then
    echo -e "\nNO Target format!!!\n"
    exit -1
fi

export -f fc-media-convert
export TO_FMT

printf '%s\0' "$@" | xargs -0 -I{} -n1 -P$(nproc) bash -c 'fc-media-convert "{}" "$TO_FMT"'
