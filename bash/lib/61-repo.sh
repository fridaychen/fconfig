# -*- mode: sh -*-

alias R='repo'
alias RA='fj-repo-forall'
alias RC='fj-repo-check'

function fj-repo-forall() {
    repo forall -c "$*"
}

function fj-repo-check() {
    local repodir=$(fc-locate-file-in-path .repo)

    fc-include ${repodir}/.repo.sh

    if [[ $# -eq 0 ]]; then
        RA fc-gitc -r ${repodir} $DEFAULT_REPO_CHECK_PARAM
    else
        RA fc-gitc -r ${repodir} $*
    fi
}
