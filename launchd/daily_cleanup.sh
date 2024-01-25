#!/bin/bash

# Logging. Sets max size, and keeps a single backup
LOG_DIR=$HOME/.dotfiles/launchd
LOG_FILE=$LOG_DIR/daily_cleanup.log

DOWNLOADS=${HOME}/Downloads
TODAY=${DOWNLOADS}/0_today
YESTERDAY=${DOWNLOADS}/1_yesterday
LAST_WEEK=${DOWNLOADS}/2_last_week
LAST_MONTH=${DOWNLOADS}/3_last_month
TRASH=${HOME}/.Trash

function cleanup_logs() {
    LOG_SIZE=0
    if [ -f $LOG_FILE ]; then
        LOG_SIZE=$(wc -l < $LOG_FILE)
    fi

    MAX_SIZE=$((8*1024*1024)) # Max size 1MB
    if [[ $LOG_SIZE -ge $MAX_SIZE ]]; then
        mv $LOG_FILE $LOG_FILE.1
    fi
}

function log() {
    echo "[$(DATE)] ${1}" # >> $LOG_FILE
}

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


function make_dirs() {
    for DIR in $@; do
        if [ ! -d $DIR ]; then
            mkdir $DIR
        fi
    done
}

function main() {
    echo ""
    log "Running daily cleanup"

    # Check that we have access to downloads
    if ! ls $DOWNLOADS >/dev/null; then
        echo "Unable to access downloads folder!"
        echo "ensure /bin/bash has full disk access"
        exit 1
    fi

    cleanup_logs
    make_dirs $TODAY $YESTERDAY $LAST_WEEK $LAST_MONTH

    # Clean up downloads folder. Move from Today -> Yesterday, Yesterday -> Last Week, and remove old entries in Last Week
    
    # Meant to be run at 4AM
    cleanup_helper $TODAY $YESTERDAY +0
    cleanup_helper $YESTERDAY $LAST_WEEK +1d
    cleanup_helper $LAST_WEEK $LAST_MONTH +7d
    cleanup_helper $LAST_MONTH $TRASH +30d

    # Empty trash
    rm -rf $TRASH/*
    log "Done"
}

main
