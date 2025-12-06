#!/bin/bash
# Deploy cosmic-comp to remote test machine and restart compositor

set -e

REMOTE_HOST="192.168.7.214"
REMOTE_USER="ericky"
REMOTE_PASS="playtron"
BINARY="cosmic-comp"
BUILD_TYPE="${1:-release}"
VIDEO_FILE="${2:-/home/ericky/veil_av1.webm}"

echo "=== Building cosmic-comp ($BUILD_TYPE) with video-wallpaper feature ==="
if [ "$BUILD_TYPE" = "debug" ]; then
    cargo build --features video-wallpaper
    BINARY_PATH="target/debug/$BINARY"
else
    cargo build --release --features video-wallpaper
    BINARY_PATH="target/release/$BINARY"
fi

echo "=== Deploying to $REMOTE_USER@$REMOTE_HOST ==="
sshpass -p "$REMOTE_PASS" scp -o StrictHostKeyChecking=no "$BINARY_PATH" "$REMOTE_USER@$REMOTE_HOST:/tmp/$BINARY"

echo "=== Installing and restarting compositor ==="
sshpass -p "$REMOTE_PASS" ssh -o StrictHostKeyChecking=no "$REMOTE_USER@$REMOTE_HOST" bash -s "$VIDEO_FILE" << 'EOF'
set -e
VIDEO_FILE="$1"

# Set up video wallpaper environment variable
# Set environment variables for the session
systemctl --user set-environment COSMIC_VIDEO_WALLPAPER="$VIDEO_FILE"
systemctl --user set-environment RUST_LOG="cosmic_comp=info,warn"
systemctl --user set-environment GST_DEBUG="3"

# Update bash_profile for session-wide env
grep -q "COSMIC_VIDEO_WALLPAPER" ~/.bash_profile 2>/dev/null && \
    sed -i "s|export COSMIC_VIDEO_WALLPAPER=.*|export COSMIC_VIDEO_WALLPAPER=$VIDEO_FILE|" ~/.bash_profile || \
    echo "export COSMIC_VIDEO_WALLPAPER=$VIDEO_FILE" >> ~/.bash_profile

# Install using atomic rename to avoid "text file busy" error
echo "Installing new binary..."
echo "playtron" | sudo -S cp /tmp/cosmic-comp /usr/bin/cosmic-comp.new
echo "playtron" | sudo -S chmod +x /usr/bin/cosmic-comp.new
echo "playtron" | sudo -S mv /usr/bin/cosmic-comp.new /usr/bin/cosmic-comp

# Kill cosmic-comp to trigger restart by session manager
echo "Restarting compositor..."
pkill -x cosmic-comp || true

echo "Done! Video set to: $VIDEO_FILE"
EOF

echo "=== Deployment complete ==="
echo "Video file: $VIDEO_FILE"
