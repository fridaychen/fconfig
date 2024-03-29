# -*- mode: sh; sh-shell: bash; -*-

function fpm-list {
    apt list --installed
}

function fpm-dump {
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

function fpm-search-file {
    dpkg -S $*
}

function fpm-update {
    sudo apt update
    sudo apt upgrade
}
