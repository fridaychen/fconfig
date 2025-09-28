#!/bin/bash

function modify_brightness_through_sysfs {
    local delta current max

    delta=$(echo "scale=4; $1 / 1000 / 1;" | bc)

    current=$(< /sys/class/backlight/*/brightness)
    max=$(< /sys/class/backlight/*/max_brightness)

    new=$(echo "scale=4; $current/(1.0 * $max)+$delta" | bc)

    new=$(echo "if ($new > 0) { $new } else { 0 }" | bc)
    new=$(echo "if ($new <= 0.99) { $new } else { 0.99 }" | bc)

    new_brightness=$(echo "($new * $max + 0.5) / 1" | bc)

    echo $new_brightness > /sys/class/backlight/*/brightness
    echo $(echo "$new*100" | bc)
}

function modify_brightness_through_hdmi {
    v=$1

    if [[ $v -gt 0 ]]; then
        ddcutil setvcp 10 + $v
    else
        ddcutil setvcp 10 - $(($v * -1))
    fi
}

function modify_brightness {
    if $(ls /sys/class/backlight/*/brightness 2> /dev/null); then
        modify_brightness_through_sysfs $*
    else
        modify_brightness_through_hdmi $*
    fi
}

function dim {
    modify_brightness -5
}

function brighten {
    modify_brightness 5
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
