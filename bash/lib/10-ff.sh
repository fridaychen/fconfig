# -*- mode: sh -*-

function ff-run {
    local pattern=${1}
    shift

    eval "ff -color ${pattern}" |
        sort -h |
        fzf-run "$@"
}

function ff-run-all {
    local pattern=${1}
    shift

    eval "ff ${pattern}" |
        sort -h |
        xargs --no-run-if-empty \
            -d "\n" \
            -I {} \
            -o "$@"
}

function ff-run-loop {
    local pattern=${1}
    shift

    eval "ff -color ${pattern}" |
        sort -h |
        fzf-run-loop "$@"
}
