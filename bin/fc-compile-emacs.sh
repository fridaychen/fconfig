#!/bin/bash

sudo apt build-dep emacs
sudo apt install libharfbuzz-dev

git clone git://github.com/emacs-mirror/emacs.git
cd emacs
./autogen.sh
./configure --with-native-compilation --with-json
make -j$(nproc)
