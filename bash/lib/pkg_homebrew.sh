# -*- mode: sh; sh-shell: bash; -*-

function fpm-list-installed {
    brew list
}

function fpm-list {
    true
}

function fpm-info {
    brew info $*
}

function fpm-search {
    brew search $*
}

function fpm-add {
    brew install $*
}

function fpm-del {
    brew uninstall $*
}

function fpm-update {
    brew update
    brew upgrade
    brew cleaniup
}
