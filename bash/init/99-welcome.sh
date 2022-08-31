# -*- mode: sh -*-

function fj-welcome() {
    cmd=""

    if fc-app-exists "fortune"; then
        cmd+="fortune"
    else
        cmd+="echo -e '\nHello world!'"
    fi

    if fc-app-exists "cowsay"; then
        cmd+=" | cowsay -W50 -f $FCHOME/extra/fc.cow"
    fi

    if fc-app-exists "lolcat"; then
        cmd+=" | lolcat -F 0.09"
    fi

    eval $cmd
}

fj-welcome
