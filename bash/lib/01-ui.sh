# -*- mode: sh; sh-shell: bash; -*-

NO_CONFIRM=false

function fc-waitkey {
    while read -e -t 0.1; do :; done
    read -n 1 -s -r -p "Press key [${*}] ❓ " opt
    echo $opt
}

function fc-user-confirm {
    if [[ $NO_CONFIRM == true ]]; then
        return 0
    fi

    while read -e -t 0.1; do :; done

    echo -n $(ansi-part $(ansi-fg $ANSI_RED))
    echo -ne "$* [y/N] ? "
    echo -n $(ansi-part $ANSI_NORMAL)

    local opt

    read -n 1 -s -r opt
    echo -e "\n"
    [[ ${opt} = "y" || ${opt} = "Y" ]]
}

function fc-clear-input {
    while read -e -t 0.1; do :; done
}

function fj-title {
    hl-msg "<(^.^)>" "$@"
}

function fj-speak {
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

function fj-done {
    if [[ $? -eq 0 ]]; then
        fj-speak ${1:-great}
    else
        fj-speak ${2:-oops}
    fi
}

function fc-user-select-with-select() {
    local prompt="$1 : "
    shift 1

    PS3=$prompt
    select x in "$@"; do
        case $x in
            *)
                echo $x
                break
                ;;
        esac
    done
}

function fc-user-select-with-fzf() {
    local prompt="$1 : "
    shift 1

    for x; do
        echo $x
    done | fzf --ansi --prompt="${prompt}" | fargs echo
}

function fc-user-select() {
    if fc-app-exists "fzf"; then
        fc-user-select-with-fzf "$@"
    else
        fc-user-select-with-select "$@"
    fi
}
