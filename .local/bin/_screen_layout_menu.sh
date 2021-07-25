#!/bin/sh

cd .screenlayout && ls -1 *.sh | rofi -dmenu -p 'Screen Layout' | xargs -r sh
