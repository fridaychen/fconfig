#!/usr/bin/env bash

source $FCHOME/bash/lib.sh

cmd=""

function arg-set {
    case $1 in
        s)
            export -f fc-media-convert-speech
            cmd='fc-media-convert-speech "{}"'
            ;;
        m)
            export -f fc-media-convert-music
            cmd='fc-media-convert-music "{}"'
            ;;
    esac
}

USAGE="Usage: audio-convert.sh [OPTION] [FILE]\n\n
  -s speech
  -m music
"

ARGUMENTS="hsm"
source $FCHOME/bash/lib/argparser.sh

if [[ -z $cmd ]]; then
    echo -e "\nNO format!!!\n"
    exit -1
fi

printf '%s\0' "$@" | xargs -0 -I{} -n1 -P$(nproc) bash -c "$cmd"
