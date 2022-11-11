#!/bin/bash

function modify_brightness {
    local delta=$1

    current=$(brightness -l | grep "display 0: brightness" | cut -d" " -f4)

    new=$(echo "$current+($delta)" | bc)

    new=$(echo "if ($new > 0) { $new } else { 0 }" | bc)
    new=$(echo "if ($new <= 0.99) { $new } else { 0.99 }" | bc)

    brightness -d 0 $new

    echo $(echo "$new*100" | bc)
}

function dim {
    modify_brightness -0.02
}

function brighten {
    modify_brightness 0.02
}

function modify_volume {
    local delta=$1

    current=$(osascript -e "output volume of (get volume settings)")

    new=$(echo "$current +($delta)" | bc)

    new=$(echo "if ($new > 0) { $new } else { 0 }" | bc)
    new=$(echo "if ($new <= 100) { $new } else { 100 }" | bc)

    osascript -e "set volume output volume ${new}"

    echo $new
}

function volume_up {
    modify_volume 10
}

function volume_down {
    modify_volume -10
}

function oldmute {
    current=$(osascript -e "output muted of (get volume settings)")

    case $current in
        true)
            osascript -e 'set volume output muted false'
            ;;

        false)
            osascript -e 'set volume output muted true'
            ;;
    esac
}

function mute {
    osascript -e 'set volume output muted true'
}

function unmute {
    osascript -e 'set volume output muted false'
}

function gateway {
    ~/.emacs.d/fconfig/mac/gateway_mac
}

function upgrade {
    brew update
    brew upgrade
    brew cleanup
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

    --upgrade)
        upgrade
        ;;

    --ummute)
        unmute
        ;;
esac
