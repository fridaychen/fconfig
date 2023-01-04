#!/usr/bin/env bash

if [[ ! -d ~/opensource/emacs ]]; then
    mkdir -p ~/opensource/emacs
fi

cd ~/opensource/emacs
fc-compile-emacs.sh -f -c $(nproc)
