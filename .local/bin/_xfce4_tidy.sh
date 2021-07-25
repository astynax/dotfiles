#!/bin/sh

# clean player list
PROP=$(xfconf-query -c xfce4-panel -l | grep 'mpris-players')
if [ ! -z "$PROP" ]; then
    xfconf-query -c xfce4-panel -p '/plugins/plugin-7/mpris-players' -s ''
    echo "mpris-players cleaned!"
fi
