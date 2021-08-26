#!/bin/bash

local function _emacsd-list() {
    local serverdir="${TMPDIR:-/tmp}/emacs${UID}"
    for file in $(find $serverdir -type s); do
        servers+=("${file##*/}")  
    done
    echo "${servers[@]}"
}

function emacsd(){
    emacsclient -c -nw $@
}

function emacsd-start(){
    emacs --daemon=$1
}

function emacsd-stop(){
    emacsclient -s $1 -e '(kill-emacs)'
}

function emacsd-stop-all(){
    for client in $(_emacsd-list); do
        emacsd-stop $client
    done
}

function emacsd-restart() {
    emacsd-stop $1;emacsd-start $1
}

function emacsd-list(){
    echo "Running emacs servers:"
    for word in $(_emacsd-list); do
        echo "  $word"
    done
}
