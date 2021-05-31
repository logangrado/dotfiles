#!/bin/bash

# Logging. Sets max size, and keeps a single backup
LOG_DIR=~/.dotfiles/launchd
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
if [ ! -d ~/Downloads/Today ]; then
    mkdir ~/Downloads/Today
fi
if [ ! -d ~/Downloads/Yesterday ]; then
    mkdir ~/Downloads/Yesterday
fi
if [ ! -d ~/Downloads/Last\ Week ]; then
    mkdir ~/Downloads/Last\ Week
fi
if [ ! -d ~/Downloads/Last\ Month ]; then
    mkdir ~/Downloads/Last\ Month
fi

# Empty trash
rm -rf ~/.Trash/*

# Clean up downloads folder. Move from Today -> Yesterday, Yesterday -> Last Week, and remove old entries in Last Week
# Meant to be run at 4AM
find ~/Downloads/Today/* -Btime +4h -exec mv "{}" ~/Downloads/Yesterday/ \;
find ~/Downloads/Yesterday/* -Btime +1d4h -exec mv "{}" ~/Downloads/Last\ Week/ \;
find ~/Downloads/Last\ Week/* -Btime +7d -exec  mv "{}" ~/Downloads/Last\ Month/ \;
find ~/Downloads/Last\ Week/* -Btime +30d -exec rm -rf "{}" \;

# NOTE: On catalina, you have to give cron full disk access. Drag /usr/sbin/cron into System Preferences -> Security & Privacy -> Privacy tab
