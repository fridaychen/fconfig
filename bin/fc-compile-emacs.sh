#!/bin/bash

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
    ./configure --with-native-compilation --with-json
    make -j$(nproc)
}

function install() {
    sudo make install
}

function update_package() {
    fj --emup
}

if [[ $(basename $(pwd)) = "emacs" && -d .git ]]; then
    fc-user-confirm "Update source code" && update
    fc-user-confirm "Compile" && compile
    fc-user-confirm "Install" && install
    fc-user-confirm "Update packages" && update_package
elif fc-user-confirm "Clone emacs"; then
    clone
else
    error-msg "Not under emacs !"
fi
