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

function fpm-help {
    echo "fpm-list   :: list installed packges"
    echo "fpm-add    :: install package"
    echo "fpm-del    :: uninstall package"
    echo "fpm-search :: search package"
    echo "fpm-update :: update packages"
    echo "fpm-dump   :: dump contents of a package"
}
