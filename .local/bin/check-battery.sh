#!/bin/bash

set -Eeuo pipefail

DANGER_LEVEL=15

PERCENT=$(\
    upower -i /org/freedesktop/UPower/devices/battery_BAT0 \
    | grep -Po '(?<=percentage:)\s+(\d+)%' \
    | sed -e 's/[ %]//g' \
    )

echo "Battery: $PERCENT%"

if [[ "$PERCENT" -lt "$DANGER_LEVEL" ]]; then
    zenity --warning --text "Charge me!" > /dev/null 2>&1
fi
