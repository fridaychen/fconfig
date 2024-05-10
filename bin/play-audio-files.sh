#!/usr/bin/env bash

source $FCHOME/bash/lib.sh

function player {
    if [[ $(which mpv) ]]; then
        mpv --really-quiet "$@" 1< /dev/null
    else
        mplayer -nolirc -really-quiet "$@" 1< /dev/null
    fi
}

function waitkey {
    while read -e -t 0.1; do :; done
    read -n 1 -s -r -p "Press key [r->run again q->quit other->done] ❓ " opt
    echo

    case ${opt} in
        r)
            return 0
            ;;

        q)
            exit 1
            ;;
        *) ;;
    esac

    return 1
}

count=0
total_count=30
span=4
interactive=false

function arg-set {
    case $1 in
        c)
            total_count=$2
            ;;
        i)
            interactive=true
            ;;
        s)
            span=$2
            ;;
        C)
            read -p "How many times do you want to run ? " total_count
            ;;
    esac
}

USAGE="Usage: play-audio-files.sh [OPTION] [FILES]\n\n
  -c total count\n
  -h usage\n
  -i interactive mode\n
  -s span between two files\n
  -C ask user for total count\n
"
ARGUMENTS="c:his:C"
source $FCHOME/bash/lib/argparser.sh

audio_files=("$@")
audio_file_amount=${#audio_files[@]}

for i in "${audio_files[@]}"; do
    [[ ! -f "${i}" ]] && echo "audio file ${i} not founnd" && exit -1
done

for ((i = 0; i < $total_count; i++)); do
    audiofile=${audio_files[$(($i % $audio_file_amount))]}

    hl-msg "Count:" $i " File:" $audiofile

    player $audiofile

    if [[ $interactive == true ]]; then
        while waitkey; do
            player $audiofile
        done
    else
        echo "Sleep ${span} seconds ..."
        sleep ${span}
    fi
done
