# -*- mode: sh; sh-shell: bash; -*-

case $(uname) in
    Darwin)
        SHMDIR=/tmp
        ;;

    Linux)
        SHMDIR=/dev/shm
        ;;
esac

function fc-dhas {
    [[ -f "${SHMDIR}/$1" ]]
}

function fc-ddel {
    for i; do
        rm -f "${SHMDIR}/$i"
    done
}

function fc-dput {
    local file="${SHMDIR}/$1"
    shift 1

    echo $* >$file
}

function fc-dget {
    local file="${SHMDIR}/$1"
    shift 1

    if [[ -f $file ]]; then
        read $* <$file
    fi
}
