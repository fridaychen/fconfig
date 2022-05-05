#!/bin/bash

CPUS=$(nproc)

. $FCHOME/bin/Fansi.sh
. ${FCHOME}/bin/Fcommon.sh

function clone() {
    git clone git://github.com/emacs-mirror/emacs.git
}

function update() {
    git pull
}

function compile() {
    git clean -xdf
    ./autogen.sh
    ./configure --with-native-compilation --with-json --with-pgtk
    time make -j$CPUS
}

function install() {
    sudo make install
}

function update_package() {
    fj --emup
}

function usage() {
    echo "Update and compile emacs"
    echo ""
    echo "  -c number of cpus will be used for compilation"
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
