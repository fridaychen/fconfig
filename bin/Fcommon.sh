#!/bin/bash

NO_CONFIRM=false
case $(uname) in
    Darwin)
	SHMDIR=/tmp
	;;

    Linux)
	SHMDIR=/dev/shm
	;;
esac

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
    if [[ $NO_CONFIRM == true ]]; then
	return 0
    fi

    local opt
    while read -e -t 0.1; do :; done
    read -n 1 -s -r -p "$* [y/N] ? " opt
    echo -e "\n"
    [[ ${opt} = "y" || ${opt} = "Y" ]]
}

function fc-dhas() {
    [[ -f "${SHMDIR}/$1" ]]
}

function fc-ddel() {
    for i in "$@"; do
        rm -f "${SHMDIR}/$i"
    done
}

function fc-dput() {
    local file="${SHMDIR}/$1"
    shift 1

    echo $* >$file
}

function fc-dget() {
    local file="${SHMDIR}/$1"
    shift 1

    if [[ -f $file ]]; then
        read $* <$file
    fi
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

function fc-wait-children() {
    while true; do
        wait -n || break
    done
}

function decolor() {
    sed -r "s/\x1B\[([0-9]{1,3}(;[0-9]{1,2})?)?[mGK]//g"
}
