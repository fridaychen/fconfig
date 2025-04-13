#!/usr/bin/env bash

exec ~/.emacs.d/site/python/bin/black - | ~/.emacs.d/site/python/bin/isort -
