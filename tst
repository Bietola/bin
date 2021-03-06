#!/bin/sh

# For `err` and `ERR_FILE`
source ~/bin/err-utils

# Usage
if [[ $# != 1 ]]; then
    echo "Usage: add-xrandr-resolution RES_WDITH RES_HEIGHT"
    exit 1
fi

{ err cvt $1 $2; } | head -n1

exit 1

# cvt creates xrandr mode info from a given resolution
err cvt $1 $2 |
    # Extract xrandr mode info
    tail -n1 | cut -d' ' -f2- |
    # Create new xrandr mode (lasts for one session)
    xargs xrandr --newmode

# Connect new res mode to active device (eDP1 is specific to richard)
DISPLAY_DEVICE="eDP1"
RES_MODE_NAME="$1x$2_60.00" # _60.00 is taken as granted and works on richard
xrandr --addmode $DISPLAY_DEVICE $RES_MODE_NAME

echo "Set added mode now? (p to preview) [y/N/p]"; read RES
if [[ $RES == 'p' ]]; then
    # Specific to richard
    DEFAULT_RES_MODE="1920x1080"

    echo "Preview started. New mode active for 5 seconds..."

    xrandr --output $DISPLAY_DEVICE --mode $RES_MODE_NAME
    sleep 5
    xrandr --output $DISPLAY_DEVICE --mode $DEFAULT_RES_MODE
elif [[ $RES == 'y' ]]; then
    echo "Applying mode permanently"

    xrandr --output $DISPLAY_DEVICE --mode $RES_MODE_NAME
else
    echo "Ok then"
fi
