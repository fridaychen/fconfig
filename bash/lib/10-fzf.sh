# -*- mode: sh; sh-shell: bash; -*-

function fargs {
    local cmd=${1}

    [[ ! ${cmd} == "*{}*" ]] && cmd="${cmd} {}"

    $XARGS --no-run-if-empty \
        -d "\n" \
        -I {} \
        -o ${cmd}
}

function fjf {
    ff "$@" |
        fzf --ansi \
            --bind=?:toggle-preview --preview-window right:wrap:hidden \
            -e \
            --preview "fj --preview {}" \
            --bind "enter:execute(fj --view {})"
}

function fzf-run {
    local cmd="${@}"

    [[ ! ${cmd} == "*{}*" ]] && cmd="${cmd} {}"

    fzf --ansi |
        $XARGS --no-run-if-empty \
            -d "\n" \
            -I {} \
            -o ${cmd}
}

function fzf-run-loop {
    local cmd=${1}

    [[ ! ${cmd} == "*{}*" ]] && cmd="${cmd} {}"

    fzf --ansi \
        --bind="enter:execute(${cmd})"
}
