#!/bin/bash

case $(uname) in
    Darwin)
        alias ls='gls --color=auto'
        alias xargs=gxargs
        alias find=gfind
        alias grep=ggrep
        alias sed=gsed
        alias du='gdu -BM'
        alias df='gdf -BM'
        alias test=gtest

        export GREP=ggrep
        export FIND=gfind
        export XARGS=gxargs

        test -r ~/.dircolors && eval "$(gdircolors -b ~/.dircolors)" || eval "$(gdircolors -b)"
        ;;
    Linux)
        export GREP=grep
        export FIND=find
        export XARGS=xargs

        alias du='du -BM'
        alias df='df -BM'

        if grep -q Microsoft /proc/version; then
            export FC_WLS=true
        fi
        ;;
    CYGWIN*)
        export GREP=grep
        export FIND=find
        export XARGS=xargs
        ;;
    *)
        export GREP=grep
        export FIND=find
        export XARGS=xargs
        ;;
esac

alias sl='ls'

# program
function fj-title() {
    hl-msg "<(^.^)>" "$@"
}

function fj-speak() {
    if app-exists say; then
        say "$*"
    elif fc-net-connected; then
        google-speak "$*"
    elif app-exists pico-tts; then
        echo "$*" | pico-tts -l en-US | aplay -q -f S16_LE -r 16 -
    elif app-exists espeak-ng; then
        if app-exists mbrola; then
            espeak-ng -s 140 -a 40 -v us-mbrola-2 "$*"
        else
            espeak-ng -s 140 -a 40 "$*"
        fi
    elif app-exists espeak; then
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

# basic
alias fj-reload=". $FCHOME/bin/FC"
alias S='sudo'

alias la='ls -AF'
alias ll='ls -AlFh'
alias lst="ll -rt"
alias lss="ll -rS"

alias lc=$(which lsd)

function lsd() {
    if [[ -z $1 ]]; then
        ls -d */
    else
        for x in $@; do
            ls -d "$x"/*/
        done
    fi
}

alias cd..='cd ..'
alias cd-='cd -'
alias cdt='cd "`find . -maxdepth 1 -type d | sed -e 1d | fzf`"'
alias cdtr='cd "`find . -type d | sed -e 1d | fzf`"'

alias tree="tree -N"

alias b=build-project
alias r=chtop
alias p='fzf --ansi'

function fzf-run() {
    local cmd=${1}

    [[ ! ${cmd} == "*{}*" ]] && cmd="${cmd} {}"

    fzf --ansi |
        xargs --no-run-if-empty \
            -d "\n" \
            -I {} \
            -o ${cmd}
}

function fzf-run-loop() {
    local cmd=${1}

    [[ ! ${cmd} == "*{}*" ]] && cmd="${cmd} {}"

    fzf --ansi \
        --bind="enter:execute(${cmd})"
}

function ff-run() {
    local pattern=${1}
    shift

    eval "ff -color ${pattern}" |
        sort -h |
        fzf-run "$@"
}

function ff-run-all() {
    local pattern=${1}
    shift

    eval "ff ${pattern}" |
        sort -h |
        xargs --no-run-if-empty \
            -d "\n" \
            -I {} \
            -o "$@"
}

function ff-run-loop() {
    local pattern=${1}
    shift

    eval "ff -color ${pattern}" |
        sort -h |
        fzf-run-loop "$@"
}

function fj-run() {
    sort -h |
        xargs --no-run-if-empty \
            -d "\n" \
            -I {} \
            -o "$@"
}

function fj-active-emacs-server() {
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

function e() {
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

function ec() {
    if [[ ! -t 0 ]]; then
        fzf-run "emacsclient -n"
    elif [[ $# -eq 0 ]]; then
        ff-run "" "emacsclient -n"
    else
        emacsclient -n "$@"
    fi
}

function install_color_scheme() {
    bash -c "$(wget -qO- https://git.io/vQgMr)"
}

# repo
alias R='repo'

function RA() {
    repo forall -c "$*"
}

# emacs
alias fj-rm-bak='find . -name "*~" -delete'
if [[ $FC_TRUECOLOR == true ]]; then
    alias fje="TERM=xterm-24bit emacs -nw --eval '(fc-after-restart)'"
else
    alias fje="emacs -nw --eval '(fc-after-restart)'"
fi

# others
alias fpltuml="java -jar ${FCHOME}/emacs/resource/plantuml.jar"

alias fj-open="fj --open"

function fjf() {
    ff "$@" |
        fzf --ansi \
            --bind=?:toggle-preview --preview-window right:wrap:hidden \
            -e \
            --preview "fj --preview {}" \
            --bind "enter:execute(fj --view {})"
}

alias v="fj --view"

if app-exists batcat; then
    alias c="batcat --color=always -n --theme OneHalfDark"
else
    alias c="bat --color=always -n --theme OneHalfDark"
fi

function workzone() {
    ansi-title "ℤ: " "$*"
}
