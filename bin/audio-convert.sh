#!/usr/bin/env bash

source $FCHOME/bash/lib.sh

cmd=""

function arg-set {
    case $1 in
        s)
            export -f fc-media-convert-speech
            cmd='fc-media-convert-speech audio-orig/"{}" "{}"'
            ;;
        m)
            export -f fc-media-convert-music
            cmd='fc-media-convert-music audio-orig/"{}" "{}"'
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

function mv-one-file {
    local dir=$(dirname "$1")
    mkdir -p audio-orig/"${dir}"
    mv "$1" audio-orig/"${dir}"
}

function mv-audio-files {
    mkdir -p audio-orig

    export -f mv-one-file
    ff -audio -nc -0 | $XARGS -0 -I{} -n1 -P$(nproc) bash -c 'mv-one-file "{}"'
}

mv-audio-files
ff -rp audio-orig -audio -nc -0 | $XARGS -0 -I{} -n1 -P$(nproc) bash -c "$cmd"
