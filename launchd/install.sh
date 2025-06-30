#!/bin/bash

set -euo pipefail
# set -x

install_macos() {
    TEMPLATE_DIR="$(cd "$(dirname "$0")" && pwd)"
    DEST_DIR="${HOME}/Library/LaunchAgents"

    mkdir -p "$DEST_DIR"

    for TEMPLATE in "$TEMPLATE_DIR"/*.plist.in; do
        BASENAME=$(basename "$TEMPLATE" .in)
        DEST="$DEST_DIR/$BASENAME"
        echo "Installing $BASENAME"

        echo "  Generate plist: $DEST"
        # Expand environment variables like $HOME
        envsubst < "$TEMPLATE" > "$DEST"

        echo "  Reloading LaunchAgent: $DEST"
        launchctl unload -w "$DEST" 2>/dev/null || true
        launchctl load -w "$DEST"
    done

    echo "Done"
}


install_linux() {
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
}

main() {
    case "$(uname -s)" in
        Darwin)
            install_macos
            ;;
        Linux)
            install_linux
            ;;
        *)
            echo "❌ Unsupported OS: $(uname -s)"
            exit 1
            ;;
    esac
}

main "$@"
