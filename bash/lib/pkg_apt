# -*- mode: sh; sh-shell: bash; -*-

function pkg-list-installed {
    apt list --installed
}

function pkg-list {
    dpkg -L $*
}

function pkg-info {
    apt info $*
}

function pkg-search {
    apt search $*
}

function pkg-add {
    sudo apt install $*
}

function pkg-del {
    sudo apt remove $*
}
