# -*- mode: sh; sh-shell: bash; -*-

# Play the remote audio
# $1 : URL of the audio
function fc-play-audio {
    if [[ $(which mpv) ]]; then
        mpv --no-video --audio-display=no --really-quiet "$@" 1< /dev/null
    else
        mplayer -nolirc -really-quiet "$@" 1< /dev/null
    fi
}

function fc-play-album {
    ls | sort -n | $XARGS -I{} mpv --no-video --audio-display=no --really-quiet "{}"
}

function fj-info {
    for x; do
        fj-title "$x"
        ffprobe -hide_banner "$x"
    done
}

function fj-mmerge {
    fj --mm "$@"
}

function fj-mcopy {
    fj --mcp "$1" "$2"
}

function fj-extracta {
    for x; do
        fj-title "$x"
        fj --exa "$x"
    done
}

function fj-play {
    if [[ $# == 0 ]]; then
        ff-run-loop -media "fj --play"
        return
    fi

    for i; do
        fj-title "$i"
        fj --play "$i"
    done
}

function fj-playa {
    if [[ $# == 0 ]]; then
        ff-run-loop "-video -audio" "fj --playa"
        return
    fi

    for i; do
        fj-title "$i"
        fj --playa "$i"
    done
}

function fj-playv {
    if [[ $# == 0 ]]; then
        ff-run-loop -video "fj --playv"
        return
    fi

    for i; do
        fj-title "$i"
        fj --playv "$i"
    done
}

function yta {
    fj --yta "$@"
}

function ytv {
    fj --ytv "$@"
}

function fj-set-snd-sink {
    pactl list short sinks |
        awk '{print $2}' |
        fzf |
        xargs pactl set-default-sink
}

# Using ffmpeg to convert format
# Args:
#  $1: input file
#  $2: output file
#  $3: new extennsion name
#  $4: audio bitrate
function fc-media-convert {
    local input out ext_name bitrate

    input="$1"
    out="$2"
    ext_name=$3
    bitrate=$4

    ffmpeg -y -hide_banner -loglevel error -i "$input" -ab $bitrate "${out%.*}.${ext_name}"
}
