# -*- mode: sh; sh-shell: bash; -*-

if [[ -z ${NODE_ICON[@]} ]]; then
    NODE_ICON=(
        ' ,___,'
        '( O,O) | '
        ' /)_)  | '
        '  " " ~~'
    )
fi

FC_CPU_OVERLOAD_TH=$(($(nproc) * 50))
FC_PS_FIT_AWK=$(< $FCHOME/bin/ps-fit.awk)

declare -g -A FC_PS_PREFIX

if [[ $FC_COLORFUL == false || $FC_EMOJI == false ]]; then
    FC_PS_PREFIX=([fit]="^" [fit_cherry]="Cherry" [ol]="OL" [ec]="NG" [exec]="took ")
else
    FC_PS_PREFIX=([fit]="" [fit_cherry]="🍒" [ol]="🔥" [ec]="❌" [exec]="took ")
fi

FC_EXITCODE_FILE=${USER}.bashexit.${FCROOTPID}
FC_EXEC_FILE=${USER}.bashtime.${FCROOTPID}

# Color definitions
# FG_USER: Cyan
# FG_AT: Red
# FG_HOST: Green
# FG_EXEC_TIME : Green
# FG_FIT : Magenta
# FG_OVERLOAD : Red
# FG_LOCAL : Blue

# construct colorful PS part with attr, fg, bg
# 256 <= number < 512 : 256colors FG
# 512 <= number : 256colors BG
function ps-part {
    local rst=""

    for x; do
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

function ps-save-exit-code {
    fc-dput $FC_EXITCODE_FILE $?
}

function last-command-result {
    local exitcode
    fc-dget $FC_EXITCODE_FILE exitcode

    if [[ $exitcode != 0 ]]; then
        echo -n "${FC_PS_PREFIX[ec]} "
    fi
}

function ps-resource-overload {
    case $(uname) in
        Darwin) ;;

        Linux)
            local cpu=$(< /proc/loadavg | awk '{n=$1*100} END {printf("%u", n);}')
            local mem=$(free -m | awk '/^Mem/ {printf("%u", 100*$3/$2);}')

            if [[ $mem -gt 70 || $cpu -gt $FC_CPU_OVERLOAD_TH ]]; then
                echo -n " ${FC_PS_PREFIX[ol]}$cpu/$mem"
            fi
            ;;
    esac
}

function ps-local {
    local dir=$(fc-locate-file-in-path .localinfo)

    if [[ (! -z ${dir}) && -f "${dir}/.localinfo" ]]; then
        echo -n " "
        cat ${dir}/.localinfo | tr -d "\r\n"
    fi

    dir=$(fc-locate-file-in-path .localsh)

    if [[ (! -z ${dir}) && -x "${dir}/.localsh" ]]; then
        echo -n " "
        cd ${dir}
        ./.localsh | tr -d "\r\n"
    fi
}

function ps-now {
    date +%H:%M:%S
}

function ps-fit-info {
    local branch=$(fit-current-branch)

    if [[ ! -z $branch ]]; then
        echo -n "${FC_PS_PREFIX[fit]} $branch"
        git status -s | awk "$FC_PS_FIT_AWK"

        if [[ -f $(fit-root)/.git/CHERRY_PICK_HEAD ]]; then
            echo -n " ${FC_PS_PREFIX[fit_cherry]}"
        fi
    fi
}

function ps-exec-start {
    # places the epoch time in ns into shared memory
    fc-dput $FC_EXEC_FILE $(date +%s.%N)
}

function ps-exec-time {
    if $(fc-dhas $FC_EXEC_FILE); then
        local endtime=$(date +%s.%N)
        local starttime=""
        fc-dget $FC_EXEC_FILE starttime
        printf "${FC_PS_PREFIX[exec]}%.2f" $(echo "scale=2; $endtime - $starttime" | bc)
        fc-ddel $FC_EXEC_FILE
    fi
}

function ps-art-l0 {
    echo "${NODE_ICON[0]}"
}

function ps-art-l1 {
    echo "${NODE_ICON[1]}"
}

function ps-art-l2 {
    echo "${NODE_ICON[2]}"
}

function ps-art-l3 {
    echo "${NODE_ICON[3]}"
}

function setup-ps {
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
    local PS_LOCAL="\$(ps-local)"
    local PS_EXEC_TIME="\$(ps-exec-time)"
    local PS_NOW="\$(ps-now)"
    local PS_ART_L0="\$(ps-art-l0)"
    local PS_ART_L1="\$(ps-art-l1)"
    local PS_ART_L2="\$(ps-art-l2)"
    local PS_ART_L3="\$(ps-art-l3)"

    export PROMPT_DIRTRIM=4

    [[ -z $NODE_ICON_FG ]] && NODE_ICON_FG=$FG_YELLOW

    PS0='$(ps-exec-start)'
    PS1='$(ps-save-exit-code)'
    PS1+=$(ps-part $RESET $HIGHLIGHT $NODE_ICON_FG $NODE_ICON_BG $PS_ART_L0 "\n")
    PS1+=$(ps-part $HIGHLIGHT $NODE_ICON_FG $NODE_ICON_BG $PS_ART_L1 $RESET ${FG_USER:-36} "\u")

    if [[ ! -z $SSH_TTY ]]; then
        PS1+=$(ps-part ${FG_AT:-31} "@" ${FG_HOST:-32} "\h")
    fi

    PS1+=$(ps-part ${FG_AT:-31} ":" ${FG_DIR:-33} "\w\n")
    PS1+=$(
        ps-part \
            $HIGHLIGHT $NODE_ICON_FG $NODE_ICON_BG $PS_ART_L2 $RESET \
            $HIGHLIGHT $PS_LAST \
            ${FG_EXEC_TIME:-32} $PS_EXEC_TIME " " \
            ${FG_FIT:-35} $PS_FIT \
            ${FG_OVERLOAD:-31} $PS_OVERLOAD \
            ${FG_LOCAL:-34} $PS_LOCAL \
            "\n"
    )

    PS1+=$(ps-part $HIGHLIGHT $NODE_ICON_FG $NODE_ICON_BG $PS_ART_L3 $RESET $HIGHLIGHT $NODE_ICON_FG $NODE_ICON_BG ">> " $RESET)

    local S="$(ps-art-l3)"
    S=$(make-string $((${#S} - 4)) '+')
    PS2=$(ps-part $RESET $HIGHLIGHT $FG_BLUE ">+${S}+('> " $RESET)
}

function ps-cleanup {
    fc-ddel $FC_EXEC_FILE $FC_EXITCODE_FILE
}

setup-ps

trap ps-cleanup EXIT
