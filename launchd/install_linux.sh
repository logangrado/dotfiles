#!/usr/bin/env sh

#!/bin/bash

set -e  # Exit immediately if a command exits with a non-zero status
set -x

SYSTEMD_DIR="/etc/systemd/system"
SCRIPT_DIR="$(pwd)/linux"

# Symlink all .service files
for service in "$SCRIPT_DIR"/*.service; do
    [ -e "$service" ] || continue
    sudo ln -sf "$service" "$SYSTEMD_DIR/$(basename "$service")"
done

# Symlink all .timer files
for timer in "$SCRIPT_DIR"/*.timer; do
    [ -e "$timer" ] || continue
    sudo ln -sf "$timer" "$SYSTEMD_DIR/$(basename "$timer")"
done

# Reload systemd to recognize new units
sudo systemctl daemon-reload

# Enable and start all timers
for timer in "$SCRIPT_DIR"/*.timer; do
    [ -e "$timer" ] || continue
    TIMER_NAME=$(basename "$timer")
    sudo systemctl enable "$TIMER_NAME"
    sudo systemctl start "$TIMER_NAME"
done

echo "All systemd services and timers have been linked, enabled, and started."
