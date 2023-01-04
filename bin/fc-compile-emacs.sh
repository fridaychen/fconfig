#!/usr/bin/env bash

CPUS=$(nproc)

source $FCHOME/bash/lib.sh

function clone {
    git clone git://github.com/emacs-mirror/emacs.git
}

function update {
    git pull
}

function compile {
    git clean -xdf
    ./autogen.sh
    case $(uname) in
        Darwin)
            ./configure \
                --with-ns \
                --with-native-compilation \
                --with-xwidgets \
                --with-mailutils \
                --with-json \
                --without-dbus \
                --without-compress-install
            ;;

        Linux)
            ./configure \
                --with-native-compilation \
                --with-json \
                --with-pgtk \
                --with-xwidgets
            ;;
    esac
    time make -j$CPUS
}

function install {
    case $(uname) in
        Darwin)
            make install
            rm -rf /Applications/Emacs.app
            mv nextstep/Emacs.app /Applications
            ;;
        Linux)
            sudo make install
            ;;
    esac
}

function update_package {
    fj --emup
}

function usage {
    echo "Update and compile emacs"
    echo ""
    echo "  -c number of cpus will be used for compilation"
    echo "  -f never prompt"
    echo ""
    exit
}

while getopts "hc:f" OPT; do
    case $OPT in
        c)
            CPUS=$OPTARG
            ;;
        f)
            NO_CONFIRM=true
            ;;
        *)
            usage
            ;;
    esac
done

case $(uname) in
    Darwin)
        export LDFLAGS="-L${BREW}/opt/libxml2/lib -L${BREW}/opt/giflib/lib -L${BREW}/opt/webp/lib -L${BREW}/opt/jpeg/lib -L${BREW}/opt/libtiff/lib"

        export CPPFLAGS="-I${BREW}/opt/libxml2/include -I${BREW}/opt/jpeg/include -I${BREW}/opt/libtiff/include -I${BREW}/opt/giflib/include"
        ;;
esac

if [[ $(basename $(pwd)) = "emacs" && -d .git ]]; then
    fc-user-confirm "Update source code" && update
    (fc-user-confirm "Compile" && compile) || exit
    (fc-user-confirm "Install" && install) || exit
    fc-user-confirm "Update packages" && update_package
elif fc-user-confirm "Clone emacs"; then
    clone
else
    error-msg "Not under emacs !"
fi
