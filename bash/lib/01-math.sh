# -*- mode: sh; sh-shell: bash; -*-

function fc-calc {
    awk "BEGIN { print $* }"
}
