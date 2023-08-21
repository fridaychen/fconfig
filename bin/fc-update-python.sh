#!/usr/bin/env bash

source $FCHOME/bash/lib.sh

fc-enable-alias

pip3 list --outdated |
    grep wheel$ |
    awk '{print $1}' |
    $XARGS --no-run-if-empty pip3 install --upgrade --user --break-system-packages
