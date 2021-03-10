#!/bin/bash

. ${FCHOME}/bin/Fcommon.sh

function fj-welcome() {
    cmd=""

    if app-exists "fortune"; then
        cmd+="fortune"
    else
        cmd+="echo -e '\nHello world!'"
    fi

    if app-exists "cowsay"; then
        cmd+=" | cowsay -W50 -f $FCHOME/extra/fc.cow"
    fi

    if app-exists "lolcat"; then
        cmd+=" | lolcat -F 0.09"
    fi

    eval $cmd
}

fj-welcome
