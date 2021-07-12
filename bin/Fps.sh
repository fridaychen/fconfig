#!/bin/bash

. ${FCHOME}/bin/Fcommon.sh

if [[ -z ${NODE_ICON[@]} ]]; then
    NODE_ICON=(
        ' ,___,'
        '( O,O) | '
        ' /)_)  | '
        '  " " ~~'
    )
fi

FC_CPU_OVERLOAD_TH=$(($(nproc) * 50))
FC_PS_FIT_AWK=$(cat $FCHOME/bin/ps-fit.awk)
if [[ $FC_COLORFUL == false || $FC_EMOJI == false ]]; then
    FC_FIT_PREFIX="^"
    FC_OVERLOAD_PREFIX="OL"
    FC_EXEC_PREFIX="!"
    FC_EXIT_FAIL="NG"
else
    FC_FIT_PREFIX=""
    FC_OVERLOAD_PREFIX="🔥"
    FC_EXEC_PREFIX="❕"
    FC_EXIT_FAIL="❌"
fi
FC_EXEC_FILE=/dev/shm/${USER}.bashtime.${FCROOTPID}

# construct colorful PS part with attr, fg, bg
# 256 <= number < 512 : 256colors FG
# 512 <= number : 256colors BG
function ps-part() {
    local rst=""

    for x in "$@"; do
        if [[ $x =~ ^[0-9]+$ ]]; then
            if [[ $x -lt 256 ]]; then
                rst+="\[\033[${x}m\]"
            elif [[ $x -lt 512 ]]; then
                rst+="\[\033[38;5;$(($x - 256))m\]"
            else
                rst+="\[\033[48;5;$(($x - 512))m\]"
            fi
        else
            rst+=$x
        fi
    done

    echo $rst
}

function ps-save-exit-code() {
    echo $? >"/dev/shm/${USER}.bashexit.${FCROOTPID}"
}

function last-command-result() {
    local exitcode=$(cat "/dev/shm/${USER}.bashexit.${FCROOTPID}")
    if [[ $exitcode != 0 ]]; then
        echo -n "${FC_EXIT_FAIL} "
    fi
}

function ps-resource-overload() {
    case $(uname) in
        Darwin) ;;

        Linux)
            local cpu=$(cat /proc/loadavg | awk '{n=$1*100} END {printf("%u", n);}')
            local mem=$(free -m | awk '/^Mem/ {printf("%u", 100*$3/$2);}')

            if [[ $mem -gt 70 || $cpu -gt $FC_CPU_OVERLOAD_TH ]]; then
                echo -n " ${FC_OVERLOAD_PREFIX}$cpu/$mem"
            fi
            ;;
    esac
}

function ps-now() {
    date +%H:%M:%S
}

function ps-fit-info() {
    local branch=$(fit-current-branch)

    if [[ ! -z $branch ]]; then
        echo -n "$FC_FIT_PREFIX $branch"

        git status -s | awk "$FC_PS_FIT_AWK"
    fi
}

function ps-exec-start() {
    # places the epoch time in ns into shared memory
    date +%s.%N >$FC_EXEC_FILE
}

function ps-exec-time() {
    if [[ -f $FC_EXEC_FILE ]]; then
        local endtime=$(date +%s.%N)
        local starttime=$(cat $FC_EXEC_FILE)
        printf "${FC_EXEC_PREFIX}%.2f" $(echo "scale=2; $endtime - $starttime" | bc)
        rm $FC_EXEC_FILE
    fi
}

function ps-art-l0() {
    echo "${NODE_ICON[0]}"
}

function ps-art-l1() {
    echo "${NODE_ICON[1]}"
}

function ps-art-l2() {
    echo "${NODE_ICON[2]}"
}

function ps-art-l3() {
    echo "${NODE_ICON[3]}"
}

function setup_ps() {
    local RESET=0
    local NORMAL=0
    local HIGHLIGHT=1
    local UNDERSCORE=4
    local BLINK=5
    local REVERSE=7

    local BG_BLACK=40
    local BG_RED=41
    local BG_GREEN=42
    local BG_YELLOW=43
    local BG_BLUE=44
    local BG_MAGENTA=45
    local BG_CYAN=46
    local BG_WHITE=47

    local FG_BLACK=30
    local FG_RED=31
    local FG_GREEN=32
    local FG_YELLOW=33
    local FG_BLUE=34
    local FG_MAGENTA=35
    local FG_CYAN=36
    local FG_WHITE=37

    local PS_LAST="\$(last-command-result)"
    local PS_FIT="\$(ps-fit-info)"
    local PS_OVERLOAD="\$(ps-resource-overload)"
    local PS_EXEC_TIME="\$(ps-exec-time)"
    local PS_NOW="\$(ps-now)"
    local PS_ART_L0="\$(ps-art-l0)"
    local PS_ART_L1="\$(ps-art-l1)"
    local PS_ART_L2="\$(ps-art-l2)"
    local PS_ART_L3="\$(ps-art-l3)"

    export PROMPT_DIRTRIM=4

    if [[ -z ${NODE_ICON_FG} ]]; then
        if [[ -z $SSH_TTY ]]; then
            NODE_ICON_FG=$FG_YELLOW
        else
            NODE_ICON_FG=$FG_MAGENTA
        fi
    fi

    PS0='$(ps-exec-start)'
    PS1='$(ps-save-exit-code)'
    PS1+=$(ps-part $HIGHLIGHT $NODE_ICON_FG $NODE_ICON_BG $PS_ART_L0 "\n")
    PS1+=$(ps-part $HIGHLIGHT $NODE_ICON_FG $NODE_ICON_BG $PS_ART_L1 $RESET $FG_CYAN "\u")

    if [[ ! -z $SSH_TTY ]]; then
        PS1+=$(ps-part $FG_RED "@" $FG_GREEN "\h")
    fi

    PS1+=$(ps-part $FG_RED ":" $FG_YELLOW "\w\n")
    PS1+=$(
        ps-part \
            $HIGHLIGHT $NODE_ICON_FG $NODE_ICON_BG $PS_ART_L2 $RESET \
            $HIGHLIGHT $PS_LAST \
            $FG_GREEN $PS_EXEC_TIME " " \
            $FG_MAGENTA $PS_FIT \
            $FG_WHITE $PS_OVERLOAD \
            "\n"
    )

    PS1+=$(ps-part $HIGHLIGHT $NODE_ICON_FG $NODE_ICON_BG $PS_ART_L3 $RESET $HIGHLIGHT $NODE_ICON_FG $NODE_ICON_BG ">> " $RESET)

    local S="$(ps-art-l3)"
    S=$(make-string $((${#S} - 4)) '+')
    PS2=$(ps-part $RESET $HIGHLIGHT $FG_BLUE ">+${S}+('> " $RESET)
}

setup_ps
