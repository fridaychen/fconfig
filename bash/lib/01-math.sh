#!/bin/bash

function fc-calc() {
    awk "BEGIN { print $* }"
}
