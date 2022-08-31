# -*- mode: sh -*-

function fc-calc() {
    awk "BEGIN { print $* }"
}
