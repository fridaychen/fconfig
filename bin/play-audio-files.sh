#!/usr/bin/env bash

source $FCHOME/bash/lib.sh

function usage() {
    cat <<-EOF
Usage:  ${0##*/} [OPTION] audio-files
    Play audio files continuous

    -c total count
    -h usage
    -i interactive mode
    -s span between two files
    -C ask user for total count
EOF

    exit
}

function player() {
    if [[ $(which mpv) ]]; then
        mpv --really-quiet "$@" 1</dev/null
    else
        mplayer -nolirc -really-quiet "$@" 1</dev/null
    fi
}

function waitkey() {
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

while getopts "c:his:C" OPTION; do
    case $OPTION in
        c)
            total_count=$OPTARG
            ;;
        h)
            usage
            ;;
        i)
            interactive=true
            ;;
        s)
            span=$OPTARG
            ;;
        C)
            read -p "How many times do you want to run ? " total_count
            ;;
        *)
            usage
            ;;
    esac
done

shift $((OPTIND - 1))

(($# < 1)) && usage

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
