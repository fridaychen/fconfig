#!/usr/bin/env bash

source $FCHOME/bash/lib.sh

music_mode=TRUE
hifi=FALSE
ext_name="opus"

declare -A audio_bitrate_def

audio_bitrate_def["opus_music"]="128k"
audio_bitrate_def["opus_music_hifi"]="192k"
audio_bitrate_def["opus_speech"]="32k"
audio_bitrate_def["opus_speech_hifi"]="48k"

audio_bitrate_def["m4a_music"]="128k"
audio_bitrate_def["m4a_music_hifi"]="192k"
audio_bitrate_def["m4a_speech"]="32k"
audio_bitrate_def["m4a_speech_hifi"]="48k"

function arg-set {
    case $1 in
        s)
            music_mode=FALSE
            ;;
        m)
            music_mode=TRUE
            ;;
        H)
            hifi=TRUE
            ;;
        M)
            ext_name="m4a"
            ;;
    esac
}

USAGE="Usage: audio-convert.sh [OPTION] [FILE]\n\n
  -s speech
  -m music
  -H Hi-Fi
  -M m4a
"

ARGUMENTS="hsmHM"
source $FCHOME/bash/lib/argparser.sh

if [[ $music_mode == TRUE ]]; then
    ab_name="${ext_name}_music"
else
    ab_name="${ext_name}_speech"
fi

if [[ $hifi == TRUE ]]; then
    ab_name="${ab_name}_hifi"
fi

export -f fc-media-convert
cmd="fc-media-convert audio-orig/\"{}\" \"{}\" ${ext_name} ${audio_bitrate_def[$ab_name]}"

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
