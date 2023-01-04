# -*- mode: sh; sh-shell: bash; -*-

case $(uname) in
    Darwin)
        alias ls='gls --color=auto'
        alias xargs=gxargs
        alias find=gfind
        alias grep=ggrep
        alias sed=gsed
        alias du='gdu -BM'
        alias df='gdf -BM'
        alias date=gdate

        alias emacs=/Applications/Emacs.app/Contents/MacOS/Emacs

        export GREP=ggrep
        export FIND=gfind
        export XARGS=gxargs

        [[ -r ~/.dircolors ]] && eval "$(gdircolors -b ~/.dircolors)" || eval "$(gdircolors -b)"
        ;;
    Linux)
        export GREP=grep
        export FIND=find
        export XARGS=xargs

        alias du='du -BM'
        alias df='df -BM'

        if grep -q Microsoft /proc/version; then
            export FC_WLS=true
        fi
        ;;
    CYGWIN*)
        export GREP=grep
        export FIND=find
        export XARGS=xargs
        ;;
    *)
        export GREP=grep
        export FIND=find
        export XARGS=xargs
        ;;
esac

alias sl='ls'

# basic
alias fj-reload="source $FCHOME/bin/FC"
alias S='sudo'

alias la='ls -AF'
alias ll='ls -AlFh'
alias lst="ll -rt"
alias lss="ll -rS"

function lsd {
    if [[ -z $1 ]]; then
        ls -d */ 2>/dev/null
    else
        for x; do
            ls -d "$x"/*/ 2>/dev/null
        done
    fi
}

alias cd..='cd ..'
alias cd-='cd -'
alias cdt='cd "`find . -maxdepth 1 -type d | sed -e 1d | fzf`"'
alias cdtr='cd "`find . -type d | sed -e 1d | fzf`"'

alias tree="tree -N"

alias b=proj-build
alias r=proj-chtop
alias p='fzf --ansi'

function gb {
    if [[ ! $(gp | grep -i " up to date") ]]; then
        proj-build
    fi
}

function fj-run {
    sort -h |
        xargs --no-run-if-empty \
            -d "\n" \
            -I {} \
            -o "$@"
}

function install_color_scheme {
    bash -c "$(wget -qO- https://git.io/vQgMr)"
}

# emacs
alias fj-clean='find . -name "*~" -delete'

if [[ $FC_TRUECOLOR == true ]]; then
    alias fje="TERM=xterm-24bit emacs -nw --eval '(fc-after-restart)'"
else
    alias fje="emacs -nw --eval '(fc-after-restart)'"
fi

# others
alias fpltuml="java -jar ${FCHOME}/emacs/resource/plantuml.jar"

alias fj-open="fj --open"

alias v="fj --view"

# bat / batcat
alias c="$(fc-find-app batcat bat) -n --theme $(if [[ $FC_LIGHT_THEME == true ]]; then echo gruvbox-light; else echo gruvbox-dark; fi)"

alias icat="kitty +kitten icat"
