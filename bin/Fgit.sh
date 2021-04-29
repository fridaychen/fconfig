#!/bin/bash

alias gp='git pull'
alias gq='git push'

alias fit-cancel="git reset @~"

function fit-amend() {
    if [[ $# == 0 ]]; then
        git commit --amend --no-edit
    else
        git commit --amend -m "$@"
    fi
}

function fit-current-branch() {
    git branch 2>/dev/null |
        sed -e '/^[^*]/d' -e 's/* \(.*\)/\1/'
}

function fit-count-changes() {
    git status --short 2>/dev/null |
        wc -l |
        sed -e "s/ //g"
}

function fit-switch-branch() {
    git branch |
        sed -e "/^\\*/d" |
        cut -b 3- |
        fzf --prompt="Select branch to change > " \
            --header="Current branch is [$(fit-current-branch)]" \
            --bind="enter:execute(git checkout {})+abort"
}

function fit-root() {
    git rev-parse --show-toplevel
}
