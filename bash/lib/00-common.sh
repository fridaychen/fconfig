# -*- mode: sh; sh-shell: bash; -*-

function fc-include {
    for x in $*; do
        [[ -f $x ]] && source "${x}"
    done
}

function fc-run-drop-in {
    local dir=$1
    local pattern=$2

    for x in $(ls $1/$2 | sort -n); do
        [[ -r $x ]] && source "${x}"
    done
}
