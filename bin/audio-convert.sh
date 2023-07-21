#!/usr/bin/env bash

source $FCHOME/bash/lib.sh

TO_FMT=""

function usage {
    echo "Convert audio"
    echo ""
    echo "  -t to format"
    echo ""
    echo "  filenames"
    echo ""
    exit
}

while getopts "t:" OPT; do
    case $OPT in
        t)
            TO_FMT=$OPTARG
            ;;
        *)
            usage
            ;;
    esac
done

shift $((OPTIND - 1))

if [[ -z $TO_FMT ]]; then
    usage
fi

export -f fc-media-convert
export TO_FMT

# printf '%s\0' "$@" | xargs -0 -I{} -n1 -P$(nproc) bash -c 'fc-media-convert "$@" "$TO_FMT"' _ {}
printf '%s\0' "$@" | xargs -0 -I{} -n1 -P$(nproc) bash -c 'fc-media-convert "{}" "$TO_FMT"'
