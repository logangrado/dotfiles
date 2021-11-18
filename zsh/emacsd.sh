#!/bin/bash

local function _emacsd-list() {
    local serverdir="${TMPDIR:-/tmp}/emacs${UID}"
    for file in $(find $serverdir -type s); do
        servers+=("${file##*/}")  
    done
    echo "${servers[@]}"
}

local function _launch-emacsd() {
    emacsclient -c -nw -s $@
}

function emacsd(){
    if [[ "$#" -lt 1 ]]; then
        echo "Missing client name"
        return
    fi

    CLIENT_NAME=$1
    RUNNING_CLIENTS=$(_emacsd-list)
    
    if echo $RUNNING_CLIENTS | grep -w $CLIENT_NAME > /dev/null; then
        _launch-emacsd $@
    else
        echo "No client named '${CLIENT_NAME}'"
        emacsd-list
        echo ""
        read "response?Start a new server? [Y/n] "
        case $response in
            [Yy][Ee][Ss] | [Yy] | "")
                emacsd-start $@
                _launch-emacsd $@
                ;;
            *)
                ;;
        esac
    fi
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
