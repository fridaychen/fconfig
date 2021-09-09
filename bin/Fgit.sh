#!/bin/bash

alias gp='git pull'
alias gq='git push'

alias fit-cancel="git reset @~"

function fit-amend() {
    if [[ $# == 0 ]]; then
        git commit --amend --no-edit
    else
        git commit --amend -m "$*"
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

function fit-select-branch() {
    git branch $@ | fzf --prompt="Select branch > "
}

function fit-select-commit() {
    git log \
        --graph \
        --date="format:%y/%m/%d %H:%M" \
        --pretty="format:│%h│%Cblue%an %Cgreen%ad%Creset │ %s" \
        --color=always \
        $@ |
        fzf -1 \
            --ansi \
            -d "│" \
            --reverse \
            --bind="?:toggle-preview" \
            --prompt="Select commit > " \
            --preview-window="bottom:40%:wrap" \
            --preview "git show --color=always {2}" |
        grep -o "[a-f0-9]\{7\}"
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

function fit-top() {
    local number=1
    if [[ $# -ne 0 ]]; then
        number=$1
    fi

    git log --oneline -${number}
}

function fit-search() {
    git log --oneline --grep "$1"
}

function fit-show() {
    fzf --ansi --reverse |
        cut -f1 -d" " |
        xargs git show
}
