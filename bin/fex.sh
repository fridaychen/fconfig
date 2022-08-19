#!/bin/bash

function usage() {
    echo "Usage:  ${0##*/} audio-files"
    exit
}

function extract() {
    local longext=${1#*.}
    local ext=${1##*.}

    case $longext in
        tar.gz | tar.bz2 | tar)
            tar xvf "$1"
            return
            ;;
    esac

    case $ext in
        bz2)
            bzip2 -d "$1"
            ;;
        gz)
            gzip -d "$1"
            ;;
        rar | RAR)
            unrar x "$1" || unar "$1"
            ;;
        zip | ZIP | 7z | 7Z)
            7z e "$1"
            ;;
	*)
	    echo "Unknown extension: $ext"
	    ;;
    esac
}

(($# < 1)) && usage

for arg; do
    [[ -f "$arg" ]] || {
        echo "File $arg does not exist"
        exit 1
    }

    extract "$arg"
done
