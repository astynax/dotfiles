#!/bin/sh

cd "$HOME/Projects/gitit.recursive.one/wikidata" \
    && git pull >/dev/null 2>&1 \
    && echo synced
