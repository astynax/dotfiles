#!/usr/bin/env bash

if [[ -z "$1" ]]; then
    echo "Usage: basher.bash <args...>"
else
    systemd-run --user --quiet --no-block --same-dir $*
fi
