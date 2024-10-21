# -*- mode: sh; sh-shell: bash; -*-

if [[ -d /opt/homebrew/bin ]]; then
    export PATH=/opt/homebrew/bin:$PATH
fi

fc-add-path ~/.local/bin/ \
    ${FCHOME}/bin \
    /usr/local/go/bin \
    ${HOME}/go/bin \
    /opt/homebrew/bin

case $(uname) in
    Darwin)
        fc-add-path /Applications/Emacs.app/Contents/MacOS/bin /Applications/Emacs.app/Contents/MacOS
        ;;

    Linux)
        export LANG="en_US.UTF-8"
        export LC_ALL="en_US.UTF-8"

        if [[ "$TERM" != "linux" ]]; then
            xcape_cnt=$(ps ax | grep -c xcape)

            if [[ $DISPLAY != "" && $xcape_cnt == 1 && $(which xcape 2> /dev/null) ]]; then
                xcape -e 'Control_L=Escape'
            fi
        fi
        ;;
esac

fc-include ~/.fclocal.rc
