#!/usr/bin/env bash

ID=$(uuidgen)
ENV_FILE=/tmp/$ID.env

if [[ -z "$1" ]]; then
    if [[ -z "$CMD" || -z "$DIR" ]]; then
        echo "CWD & DIR variables should be set"
        exit 1
    fi
    if [[ -d "$DIR" ]]; then
        cd "$DIR"
        (setsid $CMD &)
    else
        echo "Wrong DIR: $DIR"
    fi
    exit
else
    echo DIR=$(pwd) > $ENV_FILE
    echo CMD=$* >> $ENV_FILE
    systemctl --user start basher@"$ID".service
fi
