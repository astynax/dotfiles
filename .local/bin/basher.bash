#!/usr/bin/env bash

F=/tmp/basher_cmd.sh

if [[ -z "$1" ]]; then
    if [[ ! -f $F ]]; then
        echo "Usage: basher.bash cmd line to run"
        exit
    fi
    CMD=$(cat $F)
    (setsid $CMD &)
    exit
else
    echo $* > $F
    ID=$(uuidgen)
    systemctl --user start basher@"$ID".service
fi
