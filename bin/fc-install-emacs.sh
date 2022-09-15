#!/usr/bin/env bash

case $(uname) in
    Darwin)
        brew install emacs-plus@29 --with-native-comp
        ;;

    Linux)
        if [[ ! -d ~/opensource/emacs ]]; then
            mkdir -p ~/opensource/emacs
            cd ~/opensource/emacs
            fc-compile-emacs.sh
        fi

        cd ~/opensource/emacs
        fc-compile-emacs.sh -f -c $(nproc)
        ;;
esac
