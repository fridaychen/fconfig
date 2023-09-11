# -*- mode: sh; sh-shell: bash; -*-

function pkg-list-installed {
    pacman -Qe
}

function pkg-list {
    pacman -Ql $* | cut -f2- -d' '
}

function pkg-info {
    pacman -Qi $*
}

function pkg-search {
    pacman -Ss $*
}

function pkg-add {
    sudo pacman -S $*
}

function pkg-del {
    sudo pacman -R $*
}
