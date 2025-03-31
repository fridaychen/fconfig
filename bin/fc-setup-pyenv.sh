#!/usr/bin/env bash

source $FCHOME/bash/lib.sh

requirements=""

function arg-set {
    case $1 in
        r)
            requirements=$2
            ;;
    esac
}

USAGE="Usage: fc-setup-pyenv.sh [OPTION] env-path\n\n
  -r requirements file
"

ARGUMENTS="hr:"
source $FCHOME/bash/lib/argparser.sh

env_path=$1

python3 -m venv $env_path
. $env_path/bin/activate

if [ ! -z $requirements ]; then
    pip3 install -r $requirements
fi
