#!/bin/bash

function usage {
    echo "available options: "
    echo "--hhkb setup for the hhkb"
    echo "--101 setup for the normal 101 keyboard"
}

function set-lctrl-to-lsuper {
    xmodmap -e "remove control = Control_L"
    xmodmap -e "add mod4 = Control_L"
}

function set-capslock-to-rctrl {
    xmodmap -e 'keycode 66 = Control_R'
    xmodmap -e 'clear Lock'
    xmodmap -e 'add Control = Control_R'
}

if (($# == 0)); then
    usage
    exit 0
fi

while [ "$1" != "" ]; do
    case $1 in
        --hhkb)
            echo "setup for HHKB"
            setxkbmap us
            ;;
        --101)
            echo "setup for the normal 101 keyboard"
            set-capslock-to-rctrl
            set-lctrl-to-lsuper
            ;;
    esac

    shift
done
