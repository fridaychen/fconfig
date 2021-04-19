#!/bin/bash

sudo apt build-dep emacs
sudo apt install libharfbuzz-dev

git clone git://git.savannah.gnu.org/emacs.git -b feature/native-comp
cd emacs
./autogen.sh
./configure --with-native-compilation
make -j$(nproc)
