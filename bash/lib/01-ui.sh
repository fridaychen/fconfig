#!/bin/bash

NO_CONFIRM=false

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

function fc-clear-input() {
    while read -e -t 0.1; do :; done
}
