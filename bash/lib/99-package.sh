# -*- mode: sh; sh-shell: bash; -*-

case $(uname) in
    Darwin)
        source pkg_homebrew.sh
        ;;

    Linux)
        case $FC_DISTRO in
            arch)
                source pkg_pacman.sh
                ;;

            raspbian | ubuntu)
                source pkg_apt.sh
                ;;
        esac
        ;;
esac
