# -*- mode: sh -*-

ANSI_NORMAL=0
ANSI_HT=1
ANSI_UNDERSCORE=4
ANSI_BLINK=5
ANSI_REVERSE=7

ANSI_BLACK=0
ANSI_RED=1
ANSI_GREEN=2
ANSI_YELLOW=3
ANSI_BLUE=4
ANSI_MAGENTA=5
ANSI_CYAN=6
ANSI_WHITE=7

# ansi_output attr foreground background message
# attr :
#      0 normal
#      1 highlight
#      4 underscore
#      5 blink
#      7 reverse
# foreground and background
#      0 black
#      1 red
#      2 green
#      3 yellow
#      4 blue
#      5 pink
#      6 cyan
#      7 white

ANSI_FG=30
ANSI_256_FG=256
ANSI_BG=40
ANSI_256_BG=512

function ansi-fg {
    echo $(($@ + ANSI_FG))
}

function ansi-256-fg {
    echo $(($@ + ANSI_256_FG))
}

function ansi-bg {
    echo $(($@ + ANSI_BG))
}

function ansi-256-bg {
    echo $(($@ + ANSI_256_BG))
}

function ansi-part {
    local rst=""

    for x in "$@"; do
        if [[ $x =~ ^[0-9]+$ ]]; then
            if [[ $x -lt 256 ]]; then
                rst+="\033[${x}m"
            elif [[ $x -lt 512 ]]; then
                rst+="\033[38;5;$(($x - 256))m"
            else
                rst+="\033[48;5;$(($x - 512))m"
            fi
        else
            rst+="$x"
        fi
    done

    echo -en $rst
}

function ansi-pos {
    printf '\033[%d;%dH' $1 $2
}

function ansi-reset {
    printf '\033c'
}

function hl-msg {
    ansi-part ${ANSI_HT} $((ANSI_WHITE + 30)) $((ANSI_BLACK + 40)) "🏮 $*\n" $ANSI_NORMAL
}

function err-msg {
    ansi-part "\n" ${ANSI_HT} $((ANSI_WHITE + 30)) $((ANSI_BLACK + 40)) "💩 $*\n\n" $ANSI_NORMAL
}

function ansi-title {
    printf "\\033]0;$*\\007"
}

# init color related environment
if [[ $TERM == "linux" || $TERM == "dumb" ]]; then
    export FC_COLORFUL=false
else
    export FC_COLORFUL=true
fi

if [[ $COLORTERM =~ ^(truecolor|24bit)$ ]]; then
    export FC_TRUECOLOR=true
else
    export FC_TRUECOLOR=false
fi
