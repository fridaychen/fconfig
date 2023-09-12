# -*- mode: sh; sh-shell: bash; -*-

function fpm-list-installed {
    pacman -Qe
}

function fpm-list {
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
