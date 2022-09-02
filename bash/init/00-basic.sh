# -*- mode: sh; sh-shell: bash; -*-

fc-add-path ~/.local/bin/ \
    ${FCHOME}/bin \
    /usr/local/go/bin \
    ${HOME}/go/bin

case $(uname) in
    Darwin) ;;

    Linux)
        export FC_DISTRO=$(
            . /etc/os-release
            echo $ID
        )

        export LANG="en_US.UTF-8"
        export LC_ALL="en_US.UTF-8"

        if [[ "$TERM" != "linux" ]]; then
            xcape_cnt=$(ps ax | grep -c xcape)

            if [[ $DISPLAY != "" && $xcape_cnt == 1 ]]; then
                xcape -e 'Control_L=Escape'
            fi
        fi
        ;;
esac

fc-include ~/.fclocal.rc
