#!/bin/bash

# Logging. Sets max size, and keeps a single backup
LOG_DIR=$HOME/.dotfiles/launchd
LOG_FILE=$LOG_DIR/daily_cleanup.log
LOG_SIZE=0
if [ -f $LOG_FILE ]; then
    LOG_SIZE=$(wc -c $LOG_FILE)
fi
MAX_SIZE=$((8*1024*1024)) # Max size 1MB
if [[ $LOG_SIZE -ge $MAX_SIZE ]]; then
    mv $LOG_FILE $LOG_FILE.1
    rm $LOG_FILE
fi

echo "[$(DATE)] Running daily cleanup" >> $LOG_FILE

#Check for folder existence, create if none
if [ ! -d $HOME/Downloads/Today ]; then
    mkdir $HOME/Downloads/Today
fi
if [ ! -d $HOME/Downloads/Yesterday ]; then
    mkdir $HOME/Downloads/Yesterday
fi
if [ ! -d $HOME/Downloads/Last\ Week ]; then
    mkdir $HOME/Downloads/Last\ Week
fi
if [ ! -d $HOME/Downloads/Last\ Month ]; then
    mkdir $HOME/Downloads/Last\ Month
fi

# Empty trash
rm -rf $HOME/.Trash/*

function cleanup_helper() {
    SOURCE_DIR=$1
    DEST_DIR=$2
    BTIME=$3

    IFS=$'\n' # Ensure spaces in the filename don't break everything
    for SOURCE_PATH in $(find "$SOURCE_DIR"/* -Btime $BTIME -d 0); do
        SOURCE_NAME=$(basename $SOURCE_PATH)
        DEST_PATH_BASE=$DEST_DIR/$SOURCE_NAME
        i=0
        DEST_PATH=$DEST_PATH_BASE
        while [ -f "$DEST_PATH" ] || [ -d "$DEST_PATH" ] || [ -L "$DEST_PATH" ]; do
            i=$((i+1))
            DEST_PATH=${DEST_PATH_BASE}_${i}
        done

        mv "$SOURCE_PATH" "$DEST_PATH"
        
    done
}

# Clean up downloads folder. Move from Today -> Yesterday, Yesterday -> Last Week, and remove old entries in Last Week
# Meant to be run at 4AM
cleanup_helper $HOME/Downloads/Last\ Month $HOME/.Trash +30d
cleanup_helper $HOME/Downloads/Last\ Week $HOME/Downloads/Last\ Month +7d
cleanup_helper $HOME/Downloads/Yesterday $HOME/Downloads/Last\ Week +1d4h
cleanup_helper $HOME/Downloads/Today $HOME/Downloads/Yesterday +4h

# NOTE: On catalina, you have to give cron full disk access. Drag /usr/sbin/cron into System Preferences -> Security & Privacy -> Privacy tab
