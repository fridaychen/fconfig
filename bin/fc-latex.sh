#!/usr/bin/env bash

function run-latex {
    export TEXINPUTS=".:..:${FCHOME}/latex//:"
    xelatex --shell-escape ../$(basename "${1}")
}

cd $(dirname "${1}")

if [[ ! -d .bin ]]; then
    mkdir .bin
fi
cd .bin

srcname=$(basename "$1" .tex)

if [[ -f "$srcname.toc" ]]; then
    cp "$srcname.toc" "$srcname.old.toc"
fi

run-latex "$1"

if [[ ! -f "$srcname.old.toc" ]] || ! cmp -s "$srcname.toc" "$srcname.old.toc"; then
    run-latex "$1"
fi
