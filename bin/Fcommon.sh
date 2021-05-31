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

function fj-err() {
    echo -e $* 1>&2
}

function app-exists() {
    type "$1" &>/dev/null
}

# produce a string contains char $2 with length $1
function make-string() {
    head -c $1 </dev/zero | tr '\0' $2
}

function fc-net-connected() {
    [[ $(nmcli networking conn) = "full" ]]
}

function fc-user-confirm() {
    # local opt
    while read -e -t 0.1; do :; done
    read -n 1 -s -r -p "$* [y/N] ? " opt
    echo -e "\n"
    [[ ${opt} = "y" || ${opt} = "Y" ]]
}
