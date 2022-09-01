#!/bin/bash

function usage {
    echo "Usage of pb-decode.sh:
  pb-decode.sh -p proto-file -m message-type -s start-offset input-files

  Arguments:
    proto-file: protobuf file (.proto).
    message-type: message-type to decode.
    start: skip the header.
    input-files: raw data (text).
"
    exit -1
}

function decode-file {
    while read line; do
        [[ $line =~ ^# || $line =~ ^$ ]] && echo $line && continue

        echo
        echo "# $line"
        echo "$line" |
            cut -d" " -f${start}- |
            tee |
            xxd -r -p |
            protoc --proto_path=$(dirname "$proto_file") \
                --decode=$message \
                $proto_file
    done <"$1"
}

BASEDIR=$(dirname $(readlink -f $0))

proto_file=""
message=""
start="1"

[[ -f ${BASEDIR}/pb.rc ]] && . ${BASEDIR}/pb.rc

while getopts "hm:p:s:" OPTION; do
    case $OPTION in
        h)
            usage
            ;;
        m)
            message=$OPTARG
            ;;
        p)
            proto_file=$OPTARG
            ;;
        s)
            start=$OPTARG
            ;;
        *)
            usage
            ;;
    esac
done

shift $((OPTIND - 1))

if [[ -z $proto_file || -z $message ]]; then
    usage
fi

[[ ! -f $proto_file ]] && echo "proto file not found" && exit -1

for input in "$*"; do
    [[ ! -f $input ]] && echo "input file not found:" $input && exit -1

    decode-file $input
done
