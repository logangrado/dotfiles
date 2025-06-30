#!/bin/bash

set -euo pipefail

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
