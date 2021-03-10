#!/bin/bash

if [[ ! -d ~/.config ]]; then
    mkdir ~/.config
fi

if [[ ! -L ~/.config/nvim ]]; then
    ln -s ${FCHOME}/nvim ~/.config/nvim
fi
