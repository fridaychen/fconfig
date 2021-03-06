#!/bin/bash

function fj-info() {
    for i in "$@"; do
        fj-title "$i"
        ffprobe -hide_banner "$i"
    done
}

function fj-mmerge() {
    fj --mm "$@"
}

function fj-mcopy() {
    fj --mcp "$1" "$2"
}

function fj-extracta() {
    for i in "$@"; do
        fj-title "$i"
        fj --exa "$i"
    done
}

function fj-play() {
    if [[ $# == 0 ]]; then
        ff-run-loop -media "fj --play"
        return
    fi

    for i in "$@"; do
        fj-title "$i"
        fj --play "$i"
    done
}

function fj-playa() {
    if [[ $# == 0 ]]; then
        ff-run-loop -media "fj --playa"
        return
    fi

    for i in "$@"; do
        fj-title "$i"
        fj --playa "$i"
    done
}

function fj-playv() {
    if [[ $# == 0 ]]; then
        ff-run-loop -video "fj --playv"
        return
    fi

    for i in "$@"; do
        fj-title "$i"
        fj --playv "$i"
    done
}

function yta() {
    fj --yta "$@"
}

function ytv() {
    fj --ytv "$@"
}

function fj-set-snd-sink() {
    pactl list short sinks |
        awk '{print $2}' |
        fzf |
        xargs pactl set-default-sink
}
