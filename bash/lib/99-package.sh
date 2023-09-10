# -*- mode: sh; sh-shell: bash; -*-

case $(uname) in
    Darwin)
        source pkg_homebrew
        ;;

    Linux)
        case $FC_DISTRO in
            arch)
                source pkg_pacman
                ;;

            raspbian | ubuntu)
                source pkg_apt
                ;;
        esac
        ;;
esac
