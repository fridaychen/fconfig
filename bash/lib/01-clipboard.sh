#!/bin/bash

function clp-copy() {
    case $(uname) in
        Darwin)
            pbcopy
            ;;

        Linux)
            xsel -b
            ;;

    esac
}

function clp-paste() {
    case $(uname) in
        Darwin)
            pbpaste
            ;;

        Linux)
            xsel -o
            ;;

    esac
}
