#!/bin/bash

# Quickly activate virtualenvs
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

# A tool for creating and automatically signing virtualenv
function create_virtualenv
{
    NAME=${@: -1}
    
    # Create the virtualenv
    virtualenv $@
    
    # Activate and codesign
    source $NAME/bin/activate
    codesign -s "My Signing Identity" -f $NAME/bin/python
}
