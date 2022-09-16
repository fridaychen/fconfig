# -*- mode: sh; sh-shell: bash; -*-

alias gp='git pull'
alias gq='git push'
function gb {
    if [[ ! $(gp | grep -i " up to date") ]]; then
        proj-build
    fi
}

alias fit-cancel="git reset @~"

function fit-amend {
    if [[ $# == 0 ]]; then
        git commit --amend --no-edit
    else
        git commit --amend -m "$*"
    fi
}

function fit-clean {
    git clean -xdf
}

function fit-current-branch {
    if $(fit-in-work-tree); then
        git rev-parse --abbrev-ref HEAD
    fi
}

function fit-count-changes {
    git status --short 2>/dev/null |
        wc -l |
        sed -e "s/ //g"
}

function fit-select-branch {
    git branch $@ | fzf --prompt="Select branch > "
}

function fit-select-commit {
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

function fit-switch-branch {
    git branch |
        sed -e "/^\\*/d" |
        cut -b 3- |
        fzf --prompt="Select branch to change > " \
            --header="Current branch is [$(fit-current-branch)]" \
            --bind="enter:execute(git checkout {})+abort"
}

function fit-in-work-tree {
    [[ $(git rev-parse --is-inside-work-tree 2>/dev/null) == "true" ]]
}

function fit-root {
    if $(fit-in-work-tree); then
        git rev-parse --show-toplevel
    fi
}

function fit-top {
    local number=1
    if [[ $# -ne 0 ]]; then
        number=$1
    fi

    git log --oneline -${number}
}

function fit-search {
    git log --oneline --grep "$1"
}

function fit-show-obj {
    git cat-file -p $1
}

function fit-show {
    if [[ -t 0 ]]; then
        git log --oneline -1 |
            cut -f1 -d" " |
            xargs git show
    else
        fzf --ansi --reverse |
            cut -f1 -d" " |
            xargs git show
    fi
}

function fit-cherry-pick {
    git cherry-pick --strategy=recursive -X theirs $@
}
