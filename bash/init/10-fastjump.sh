#!/bin/bash

if app-exists fasd; then
    eval "$(fasd --init auto)"
    export _FASD_FUZZY=16
    alias j='fasd_cd -d'
elif [[ -f /usr/share/autojump/autojump.bash || -f /usr/share/autojump/autojump.sh || -f /usr/local/share/autojump/autojump.bash ]]; then
    fc-include \
        /usr/share/autojump/autojump.bash \
        /usr/share/autojump/autojump.sh \
        /usr/local/share/autojump/autojump.bash
elif [[ -f /usr/share/z/z.sh ]]; then
    fc-include /usr/share/z/z.sh
    alias j=z
fi
