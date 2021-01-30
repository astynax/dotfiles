#!/bin/bash

set -Eeuo pipefail

DANGER_LEVEL=15

PERCENT=$(\
    upower -i /org/freedesktop/UPower/devices/battery_BAT0 \
    | grep -Po '(?<=percentage:)\s+(\d+)%' \
    | sed -e 's/[ %]//g' \
    )

echo "Battery: $PERCENT%"

if [[ "$1" == "cron" && "$PERCENT" -lt 90 ]]; then
    echo "$(date -Iseconds),$PERCENT" >> $HOME/.battery.csv
fi

if [[ "$PERCENT" -lt "$DANGER_LEVEL" ]]; then
    notify-send --urgency=critical "Charge me!"
fi
