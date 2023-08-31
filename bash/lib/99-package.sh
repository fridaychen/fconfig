function pkg_list_installed {
    case $FC_DISTRO in
        arch)
            pacman -Qe
            ;;
        raspbian | ubuntu)
            apt list --installed
            ;;
    esac
}

function pkg_list {
    case $FC_DISTRO in
        arch)
            pacman -Ql $* | cut -f2- -d' '
            ;;
        raspbian | ubuntu)
            dpkg -L $*
            ;;
    esac
}

function pkg_info {
    case $FC_DISTRO in
        arch)
            pacman -Qi $*
            ;;
        raspbian | ubuntu)
            apt info $*
            ;;
    esac
}

function pkg_search {
    case $FC_DISTRO in
        arch)
            pacman -Ss $*
            ;;
        raspbian | ubuntu)
            apt search $*
            ;;
    esac
}
