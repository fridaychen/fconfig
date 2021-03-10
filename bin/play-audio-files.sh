#!/bin/bash

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

audio_files=("$@")
audio_file_amount=${#audio_files[@]}

if [[ ${audio_file_amount} -eq 0 ]]; then
    echo -e "\nPlease specify audio files !!!"
    exit -1
fi

while [[ $count -lt $total_count ]]; do
    audiofile=${audio_files[$(($count % $audio_file_amount))]}
    hl-msg "Count:" $count " File:" $audiofile

    [[ ! -f "${audiofile}" ]] && echo "file not founnd ${audiofile}" && exit -1

    player $audiofile

    if [[ $anykey2continue == true ]]; then
        while read -e -t 0.1; do :; done
        read -n 1 -s -r -p "Press key [r->run again q->quit other->done] ❓ " opt
        echo

        case ${opt} in
            r)
                continue
                ;;

            q)
                exit 0
                ;;
            *) ;;
        esac
    else
        echo "Sleep ${span} seconds ..."
        sleep ${span}
    fi

    ((count++))
done
