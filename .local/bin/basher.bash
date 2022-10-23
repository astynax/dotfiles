#!/usr/bin/env bash

F=/tmp/basher_cmd.sh

if [[ -z "$1" ]]; then
    if [[ ! -f $F ]]; then
        echo "Usage: basher.bash cmd line to run"
        exit
    fi
    mapfile -t LINES < $F
    DIR=${LINES[0]}
    if [[ -d "$DIR" ]]; then
        cd "$DIR"
        CMD=${LINES[1]}
        (setsid $CMD &)
    else
        echo "Wrong CWD: $DIR"
    fi
    exit
else
    pwd > $F
    echo $* >> $F
    ID=$(uuidgen)
    systemctl --user start basher@"$ID".service
fi
