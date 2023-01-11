# -*- mode: sh; sh-shell: bash; -*-

function fc-include {
    for x; do
        [[ -r $x ]] && source "$x"
    done
}

function fc-run-drop-in {
    local dir=$1
    local pattern=$2

    for x in $(ls $1/$2 | sort -n); do
        [[ -r $x ]] && source "$x"
    done
}

# enable interpretation alias
function fc-enable-alias {
    shopt -s expand_aliases
}
