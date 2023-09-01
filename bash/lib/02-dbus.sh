# 1 Service
# 2 Path
# 3 Interface
# 4 Property
function dbus_getp {
    dbus-send --system --dest="$1" --print-reply=literal "$2" \
              org.freedesktop.DBus.Properties.Get string:"$3" string:"$4"
}

function dbus_setp {
    return
}

function dbus_net_connected {
    dbus_getp org.freedesktop.NetworkManager \
              /org/freedesktop/NetworkManager \
              org.freedesktop.NetworkManager \
              State
}

function dbus_dump {
    dbus-send --print-reply=literal --system --dest=$1 $2 \
              org.freedesktop.DBus.Introspectable.Introspect |
        xmllint --format -
}
