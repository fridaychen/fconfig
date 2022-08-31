# -*- mode: sh -*-

if [[ ! :$PYTHONPATH: == *:${FCHOME}/python:* ]]; then
    export PYTHONPATH=$PYTHONPATH:${FCHOME}/python
fi
