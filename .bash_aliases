#!/bin/bash
#alias mp3gain_subs="for a in *; do pushd \"$a\"; mp3gain -a *.mp3; popd; done"

# some more ls aliases
alias ll='ls -AlF --color --time-style=long-iso'
alias la='ls -A'
alias l='ls -f1'

alias playlist-dl-audio="youtube-dl -x -o \"%(playlist_title)s/%(playlist_index)s. %(title)s.%(ext)s\""
alias playlist-dl="youtube-dl -o \"%(playlist_title)s/%(playlist_index)s. %(title)s.%(ext)s\""

alias lsmake="grep --color=never -oP \"^\S+:\" Makefile"

alias ec="emacsclient -nq"

alias psg="ps -eLF | grep"

alias clbin="curl -F 'clbin=<-' https://clbin.com"
alias termbin="nc termbin.com 9999"

alias cal="ncal -3bM"

htags() {
    hasktags -e $(find `pwd` -iname '*.hs' -and \( -not -path '*.stack-work*' \) -and \( -not -name 'Setup.hs' \) -and -type f)
}

