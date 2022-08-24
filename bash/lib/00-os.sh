#!/bin/bash

function fc-copy() {
    local src=$1
    local target=$2

    [[ ! -f "$target" || ! ("$target" -nt "$src") ]] &&
        mkdir -p $(dirname "$target") &&
        cp "$src" "$target"
}

function fc-add-path() {
    if [[ -d $1 && ! :$PATH: == *:$1:* ]]; then
        export PATH=$PATH:$1
    fi
}

function app-exists() {
    type "$1" &>/dev/null
}

function fc-net-connected() {
    case $(uname) in
        Darwin)
            /System/Library/PrivateFrameworks/Apple80211.framework/Versions/A/Resources/airport -I | grep running >/dev/null
            ;;
        Linux)
            [[ $(nmcli networking conn) = "full" ]]
            ;;
    esac
}

function fc-wait-children() {
    while true; do
        wait -n || break
    done
}

function fc-locate-file-in-path() {
    local dir="$(realpath .)"

    while [[ "${dir}" != "/" ]]; do
        for i in "$@"; do
            if [[ -f "${dir}/$i" || -d "${dir}/$i" ]]; then
                echo ${dir}
                return
            fi
        done

        dir=$(dirname "${dir}")
    done
}