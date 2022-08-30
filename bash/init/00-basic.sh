#!/bin/bash

fc-add-path ~/.local/bin/ \
    ${FCHOME}/bin \
    /usr/local/go/bin \
    ${HOME}/go/bin

case $(uname) in
    Darwin) ;;

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

alias info=pinfo
