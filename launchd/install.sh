#!/bin/bash

DEST_DIR=${HOME}/Library/LaunchAgents
for SOURCE in $(find . -name "*.plist" -exec realpath {} \;); do
    SOURCE_NAME=$(basename $SOURCE)
    DEST=${DEST_DIR}/${SOURCE_NAME}

    if [[ -L $DEST ]]; then
        rm $DEST
    fi
    
    echo -n "  Linking... "
    ln -s $SOURCE $DEST
    echo "done"

    echo -n "  Loading... "
    launchctl unload -w $DEST
    launchctl load -w $DEST
    echo "done"
done
