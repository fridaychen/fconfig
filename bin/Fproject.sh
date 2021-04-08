#!/bin/bash

function build-project() {
    local old_dir=$(pwd)

    if ! chtop; then
        hl-msg "NO TOP, try to do it at here."
    fi

    hl-msg "TOP at $(pwd)"

    if [[ -f CMakeLists.txt && ! -d build ]]; then
        cmake-project
    fi

    if [[ -f build/build.ninja ]]; then
        cd build
        time ninja $*
    elif [[ -f build/Makefile ]]; then
        cd build
        time make $*
    elif [[ -f SConstruct || -f sconstruct ]]; then
        time scons $*
    elif [[ -f GNUmakefile || -f Makefile || -f makefile ]]; then
        time make $*
    elif [[ -f PKGBUILD ]]; then
        makepkg -si
    else
        error-msg "DO NOT know how to build\n"
    fi

    fj-done "great" "oops"
    cd "$old_dir"
}

function chtop() {
    local old_dir=$(pwd)

    while [[ (! (-f TOP || -f .TOP)) && $(pwd) != "/" ]]; do
        cd ..
    done

    if [[ ! (-f TOP || -f .TOP) ]]; then
        ansi-output $ANSI_BLINK $ANSI_YELLOW $ANSI_RED "failed to find TOP"
        echo
        cd $old_dir
        return 1
    fi

    return 0
}

function create-project() {
    local name=$1

    mkdir $name
    mkdir $name/doc
    mkdir $name/src

    touch $name/.TOP
    touch $name/notes
}

function cmake-project() {
    mkdir build
    cd build
    cmake .. -DCMAKE_EXPORT_COMPILE_COMMANDS=ON -G Ninja
    cd ..
}
