# -*- mode: sh; sh-shell: bash; -*-

function fpm-list {
    pacman -Qe
}

function fpm-dump {
    pacman -Ql $* | cut -f2- -d' '
}

function fpm-info {
    pacman -Qi $*
}

function fpm-search {
    pacman -Ss $*
}

function fpm-add {
    sudo pacman -S $*
}

function fpm-del {
    sudo pacman -R $*
}

function fpm-search-file {
    pacman -F $*
}

function fpm-update {
    sudo pacman -Syu
}
