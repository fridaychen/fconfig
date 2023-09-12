# -*- mode: sh; sh-shell: bash; -*-

function fpm-list-installed {
    apt list --installed
}

function fpm-list {
    dpkg -L $*
}

function fpm-info {
    apt info $*
}

function fpm-search {
    apt search $*
}

function fpm-add {
    sudo apt install $*
}

function fpm-del {
    sudo apt remove $*
}
