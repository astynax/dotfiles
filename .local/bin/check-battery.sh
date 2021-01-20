#!/bin/sh

DANGER_LEVEL=15

PERCENT=$(\
    upower -i /org/freedesktop/UPower/devices/battery_BAT0 \
    | grep -Po '(?<=percentage:)\s+(\d+)%' \
    | sed -e 's/[ %]//g' \
    )

if [ "$PERCENT" -lt "$DANGER_LEVEL" ]; then
    zenity --warning --text "Charge me!" 2>/dev/null
fi
