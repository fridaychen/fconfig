#!/bin/bash

ANSI_NORMAL=0
ANSI_HIGHLIGHT=1
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

# Ansi output function
# parameter
#  $1 : attribute
#  $2 : foreground
#  $3 : background
#  $4.. : contents
function ansi-output() {
    if [[ (! -t 1) || -z $FC_COLORFUL || $FC_COLORFUL == false ]]; then
        shift 3
        format=$1
        shift
        printf "$format" $*
    else
        printf '\033[%s;%s;%sm' $1 $(($2 + 30)) $(($3 + 40))
        shift 3
        format=$1
        shift
        printf "$format" $*
        printf '\033[0;m'
    fi

    #printf '\033[%s;%s;%sm%s\033[0m' $1 $(($2 + 30)) $(($3 + 40)) $4
}

function ansi-pos() {
    printf '\033[%d;%dH' $1 $2
}

function ansi-test() {
    for attr in 0 1 4 5 7; do
        for fore in 30 31 32 33 34 35 36 37; do
            for back in 40 41 42 43 44 45 46 47; do
                printf '\033[%s;%s;%sm %02s;%02s ' $attr $fore $back $fore $back
            done

            printf "\n"
        done

        printf '\033[0;m'
    done
}

function ansi-reset() {
    printf '\033c'
}

function hl-msg() {
    ansi-output $ANSI_HIGHLIGHT $ANSI_BLUE $ANSI_BLACK ""
    ansi-output $ANSI_HIGHLIGHT $ANSI_WHITE $ANSI_BLUE " $* "
    ansi-output $ANSI_HIGHLIGHT $ANSI_BLUE $ANSI_BLACK ""
    echo
}

function error-msg() {
    ansi-output $ANSI_BLINK $ANSI_WHITE $ANSI_RED "ERROR >> "
    ansi-output $ANSI_HIGHLIGHT $ANSI_WHITE $ANSI_MAGENTA "$*"

    echo
}

function ansi-title() {
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
