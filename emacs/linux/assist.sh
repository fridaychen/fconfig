#!/bin/bash

function modify_brightness {
    local delta=$1 current max

    current=$(</sys/class/backlight/*/brightness)
    max=$(</sys/class/backlight/*/max_brightness)

    new=$(echo "scale=4; $current/(1.0 * $max)+$delta" | bc)

    new=$(echo "if ($new > 0) { $new } else { 0 }" | bc)
    new=$(echo "if ($new <= 0.99) { $new } else { 0.99 }" | bc)

    new_brightness=$(echo "($new * $max + 0.5) / 1" | bc)

    echo $new_brightness >/sys/class/backlight/*/brightness
    echo $(echo "$new*100" | bc)
}

function dim {
    modify_brightness -0.005
}

function brighten {
    modify_brightness 0.005
}

function volume_up {
    pactl set-sink-volume 0 +10%
}

function volume_down {
    pactl set-sink-volume 0 -10%
}

function mute {
    pamixer -m
}

function unmute {
    pamixer -u
}

function gateway {
    ~/.emacs.d/fconfig/linux/gateway_mac
}

function upgrade {
    sudo apt update
    sudo apt upgrade
}

function pmsleep {
    systemctl suspend -i
}

case $1 in
    --dim)
        dim
        ;;

    --brighten)
        brighten
        ;;

    --volup)
        volume_up
        ;;

    --voldown)
        volume_down
        ;;

    --mute)
        mute
        ;;

    --gateway)
        gateway
        ;;

    --pmsleep)
        pmsleep
        ;;

    --upgrade)
        upgrade
        ;;

    --unmute)
        unmute
        ;;
esac
