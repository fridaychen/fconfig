# -*- mode: sh -*-

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

function fj-title() {
    hl-msg "<(^.^)>" "$@"
}

function fj-speak() {
    if fc-app-exists say; then
        say "$*"
    elif fc-net-connected; then
        google-speak "$*"
    elif fc-app-exists pico-tts; then
        echo "$*" | pico-tts -l en-US | aplay -q -f S16_LE -r 16 -
    elif fc-app-exists espeak-ng; then
        if fc-app-exists mbrola; then
            espeak-ng -s 140 -a 40 -v us-mbrola-2 "$*"
        else
            espeak-ng -s 140 -a 40 "$*"
        fi
    elif fc-app-exists espeak; then
        espeak "$*"
    fi
}

function fj-done() {
    if [[ $? -eq 0 ]]; then
        fj-speak ${1:-great}
    else
        fj-speak ${2:-oops}
    fi
}
