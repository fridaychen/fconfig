#!/bin/bash

fc-include ~/.fclocal.rc

# init fast jump
if app-exists fasd; then
    eval "$(fasd --init auto)"
    export _FASD_FUZZY=16
    alias j='fasd_cd -d'
elif [[ -f /usr/share/autojump/autojump.bash || -f /usr/share/autojump/autojump.sh || -f /usr/local/share/autojump/autojump.bash ]]; then
    fc-include \
        /usr/share/autojump/autojump.bash \
        /usr/share/autojump/autojump.sh \
        /usr/local/share/autojump/autojump.bash
elif [[ -f /usr/share/z/z.sh ]]; then
    fc-include /usr/share/z/z.sh
    alias j=z
fi

# init fzf
fc-include \
    ~/.fzf.bash \
    /usr/share/fzf/completion.bash \
    /usr/share/fzf/key-bindings.bash \
    /usr/share/doc/fzf/examples/completion.bash \
    /usr/share/doc/fzf/examples/key-bindings.bash

fc-add-path ${FCHOME}/bin

case $(uname) in
    Darwin)
        ;;

    Linux)
        export FC_DISTRO=$(
            . /etc/os-release
            echo $ID
        )

        export LANG="en_US.UTF-8"
        export LC_ALL="en_US.UTF-8"

        if [[ "$TERM" != "linux" ]]; then
            xcape_cnt=$(ps ax | grep -c xcape)

            if [[ $DISPLAY != "" && $xcape_cnt == 1 ]]; then
                xcape -e 'Control_L=Escape'
            fi
        fi
        ;;
esac

export PAGER=less

# colorful man page
export MANPAGER=less
export LESS_TERMCAP_mb=$'\e[1;32m'
export LESS_TERMCAP_md=$'\e[1;32m'
export LESS_TERMCAP_me=$'\e[0m'
export LESS_TERMCAP_se=$'\e[0m'
export LESS_TERMCAP_so=$'\e[01;33m'
export LESS_TERMCAP_ue=$'\e[0m'
export LESS_TERMCAP_us=$'\e[1;4;31m'

export BAT_PAGER="less -RF"

fc-add-path ${HOME}/.local/bin

#
alias info=pinfo

# go
fc-add-path /usr/local/go/bin
fc-add-path ${HOME}/go/bin

# python
if [[ ! :$PYTHONPATH: == *:${FCHOME}/python:* ]]; then
    export PYTHONPATH=$PYTHONPATH:${FCHOME}/python
fi

# rust
fc-add-path ${HOME}/.cargo/bin

# bash
export EDITOR=nvim

function _fcedit() {
    nvim +'set ft=sh' "$@"
}

export FCEDIT=_fcedit

fc-copy ${FCHOME}/bin/.dircolors ~/.dircolors

export FCROOTPID=$BASHPID
