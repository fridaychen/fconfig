# -*- mode: sh -*-

function proj-build() {
    local old_dir=$(pwd)

    if ! proj-chtop; then
        hl-msg "NO TOP, try to do it at here."
    else
        hl-msg "TOP at $(pwd)"
    fi

    if [[ -f CMakeLists.txt && ! -d build ]]; then
        proj-cmake
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
    elif [[ -x ./build.sh ]]; then
        ./build.sh
    else
        err-msg "DO NOT know how to build"
    fi

    fj-done "great" "oops"
    cd "$old_dir"
}

function proj-root() {
    fc-locate-file-in-path ".TOP" ".top"
}

function proj-chtop() {
    local root=$(proj-root)

    if [[ -z ${root} ]]; then
        err-msg "failed to find TOP"
        return 1
    fi

    cd $root
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

function proj-cmake() {
    mkdir build
    cd build
    cmake .. -DCMAKE_EXPORT_COMPILE_COMMANDS=ON -G Ninja
    cd ..
}
