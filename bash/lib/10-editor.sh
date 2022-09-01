# -*- mode: sh -*-

function fj-active-emacs-server {
    local emacs_servers=(
        "/var/run/user/${UID}/emacs/server"
        "/tmp/emacs${UID}/server"
        "${TMPDIR}/emacs${UID}/server"
    )

    for i in ${emacs_servers[@]}; do
        [[ -S $i ]] && return 0
    done

    return 1
}

function e {
    if fj-active-emacs-server; then
        ec "$@"
    elif [[ ! -t 0 ]]; then
        fzf-run nvim
    elif [[ $# -eq 0 ]]; then
        ff-run "" nvim
    else
        nvim "$@"
    fi
}

function ec {
    if [[ ! -t 0 ]]; then
        fzf-run "emacsclient -n"
    elif [[ $# -eq 0 ]]; then
        ff-run "" "emacsclient -n"
    else
        emacsclient -n "$@"
    fi
}
