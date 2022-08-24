#!/bin/bash

if [[ ! :$PYTHONPATH: == *:${FCHOME}/python:* ]]; then
    export PYTHONPATH=$PYTHONPATH:${FCHOME}/python
fi
