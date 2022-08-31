# -*- mode: sh -*-

function fc-copy() {
    local src=$1
    local target=$2

    [[ ! -f "$target" || ! ("$target" -nt "$src") ]] &&
        mkdir -p $(dirname "$target") &&
        cp "$src" "$target"
}

function fc-add-path() {
    for x in "$@"; do
        if [[ -d "$x" && ! :$PATH: == *:"$x":* ]]; then
            export PATH=$PATH:"$x"
        fi
    done
}

function fc-app-exists() {
    type "$1" &>/dev/null
}

function fc-find-app() {
    for x; do
        if fc-app-exists $x; then
            echo $x
            return
        fi
    done
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
