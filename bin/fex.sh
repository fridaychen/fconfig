#!/usr/bin/env bash

VERBOSE=:

function usage {
    cat <<- EOF
Usage:  ${0##*/} [OPTION] package-files
    Extract packages into current directory.

    -h usage
    -v verbose
EOF

    exit
}

function extract {
    local longext=${1#*.}
    local ext=${1##*.}

    case ${longext,,} in
        tar | tar.bz2 | tar.gz | tar.xz)
            tar xvf "$1"
            return
            ;;
    esac

    case ${ext,,} in
        bz2)
            bzip2 -d "$1"
            ;;
        gz)
            gzip -d "$1"
            ;;
        rar | zip | 7z)
            7z x "$1"
            ;;
        *)
            echo "Unknown extension: $ext"
            ;;
    esac
}

function arg-set {
    case $1 in
        v)
            VERBOSE='echo'
            ;;
    esac
}

USAGE="Usage: fex.sh [OPTION] [FILES]\n\n
  -v verbose\n
"
ARGUMENTS="hv"
source $FCHOME/bash/lib/argparser.sh

for arg; do
    [[ -f "$arg" ]] || {
        echo "File $arg does not exist"
        exit 1
    }

    $VERBOSE "Extracting $arg ..."
    extract "$arg"
done
