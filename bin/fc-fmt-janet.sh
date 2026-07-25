#!/usr/bin/env bash

tmpfile=$(mktemp) && cat - > ${tmpfile}

trap 'rm -f "$tmpfile"' EXIT

janet-format -i $tmpfile
