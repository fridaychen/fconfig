# 1 Service
# 2 Path
# 3 API
function dbus-call {
    local service=$1
    local path=$2
    local api=$3

    shift 3

    dbus-send --system --dest=$service --print-reply=literal $path $api $*
}

# 1 Service
# 2 Path
# 3 Interface
# 4 Property
function dbus-getp {
    dbus-call "$1" "$2" org.freedesktop.DBus.Properties.Get string:"$3" string:"$4"
}

function dbus-setp {
    dbus-call "$1" "$2" org.freedesktop.DBus.Properties.Set string:"$3" string:"$4" $5
}

function dbus-net-connected {
    dbus-getp org.freedesktop.NetworkManager \
              /org/freedesktop/NetworkManager \
              org.freedesktop.NetworkManager \
              State
}

function dbus-dump {
    dbus-send --print-reply=literal --system --dest=$1 $2 \
              org.freedesktop.DBus.Introspectable.Introspect |
        xmllint --format -
}

function dbus-get-ntp-server {
    dbus-getp \
        org.freedesktop.timesync1 \
        /org/freedesktop/timesync1 \
        org.freedesktop.timesync1.Manager \
        ServerName
}

function dbus-set-ntp-server {
    dbus-call org.freedesktop.timesync1 \
        /org/freedesktop/timesync1 \
        "org.freedesktop.timesync1.Manager.SetRuntimeNTPServers" \
        array:string:$1
}
