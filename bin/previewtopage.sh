#!/bin/bash

if [[ -z $2 ]]; then
    [[ -z $1 ]] && printf "\n Missing Filename..."
    printf "\n Missing Page Number...\n\n"
    printf "   Syntax: OpenToPage Filename Page_Number\n"
    printf "   Example: OpenToPage Filename.pdf 3\n\n"
    exit 1
else
    open -a Preview "$1"
    osascript -e 'tell application "Preview" to activate' \
        -e 'tell application "System Events" to tell process "Preview" to click menu item "Go to Page…" of menu "Go" of menu bar 1' \
        -e "tell application \"System Events\" to keystroke \"$2\"" \
        -e 'tell application "System Events" to key code 36'
fi
exit 0
