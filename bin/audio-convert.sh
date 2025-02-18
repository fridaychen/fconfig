#!/usr/bin/env bash

source $FCHOME/bash/lib.sh

music_mode=TRUE
cmd=""
ext_name="opus"

function arg-set {
    case $1 in
        s)
            export -f fc-media-convert-speech
            cmd='fc-media-convert-speech audio-orig/"{}" "{}" ${ext_name}'
            music_mode=FALSE
            ;;
        m)
            export -f fc-media-convert-music
            cmd='fc-media-convert-music audio-orig/"{}" "{}" ${ext_name}'
            music_mode=TRUE
            ;;
        A)
            export ext_name
            ext_name="m4a"
            if [[ $music_mode == TRUE ]]; then
                export MEDIA_BITRATE=192k
            fi
            ;;
        M)
            export ext_name
            ext_name="mp3"
            if [[ $music_mode == TRUE ]]; then
                export MEDIA_BITRATE=256k
            fi
            ;;
    esac
}

USAGE="Usage: audio-convert.sh [OPTION] [FILE]\n\n
  -s speech
  -m music
  -A aac
  -M mp3
"

ARGUMENTS="hsmAM"
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

ff -x audio-orig -nc -audio -0 | $XARGS -0 -I{} -n1 -P$(nproc) ftag --split-artists "{}"
