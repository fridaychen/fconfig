#!/bin/bash

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
