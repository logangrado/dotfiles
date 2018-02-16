#!/bin/bash

function activate
{
    if [ "$#" -ne 1 ]; then
	echo "Usage: $0 PYENV" >&2
	return 1
    fi
    if ! [ -e "$1/bin/activate" ]; then
	echo "$1 not a PYENV"
	return 1
    fi
   
    source $1/bin/activate
}
