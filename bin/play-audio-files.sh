#!/usr/bin/env bash

function usage() {
    echo "
play audio files continuous

play options audio-files

-a any key to continue
-c total count
-s span between two files
"

    exit -1
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

BASEDIR=$(dirname $(readlink -f $0))

. ${BASEDIR}/Fansi.sh

count=0
total_count=30
span=4
anykey2continue=false

while getopts "ac:hs:" OPTION; do
    case $OPTION in
        a)
            anykey2continue=true
            ;;
        c)
            total_count=$OPTARG
            ;;
        h)
            usage
            ;;
        s)
            span=$OPTARG
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

for ((i=0; i < $total_count; i++)); do
    audiofile=${audio_files[$(($i % $audio_file_amount))]}

    hl-msg "Count:" $i " File:" $audiofile

    player $audiofile

    if [[ $anykey2continue == true ]]; then
	while waitkey; do
	    player $audiofile
	done
    else
        echo "Sleep ${span} seconds ..."
        sleep ${span}
    fi
done
