#!/bin/bash

export PATH=$PATH:~/.emacs.d/fconfig/win

function modify_brightness {
    nircmd changebrightness $1
}

function dim {
    modify_brightness -10
}

function brighten {
    modify_brightness 10
}

function modify_volume {
    nircmd changesysvolume $(($1 * 655))
}

function volume_up {
    modify_volume 10
}

function volume_down {
    modify_volume -10
}

function mute {
    nircmd mutesysvolume 2
}

function setappvol {
    vol=$( echo "scale=2; $1 / 100.0" | bc )

    echo $vol

    nircmd setappvolume "foobar2000.exe" $vol
}

function gateway {
    ~/.emacs.d/fconfig/win/gateway_mac
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

    --setappvol)
	setappvol $2
	;;

    --gateway)
	gateway
	;;
esac
