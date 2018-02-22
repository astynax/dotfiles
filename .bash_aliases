#!/bin/bash
#alias mp3gain_subs="for a in *; do pushd \"$a\"; mp3gain -a *.mp3; popd; done"

# some more ls aliases
alias ll='ls -AlF --color --time-style=long-iso'
alias la='ls -A'
alias l='ls -f1'

alias playlist-dl="youtube-dl -o '~/Downloads/youtube/%(uploader)s/%(playlist_mb_title)s/%(playlist_index)s. %(title)s.%(ext)s'"

alias lsmake="grep --color=never -oP \"^\S+:\" Makefile"

alias e="emacs"
alias ec="emacsclient -nq"
alias eq="emacs -Q"

alias psg="ps -eLF | grep"

alias clbin="curl -F 'clbin=<-' https://clbin.com"
alias termbin="nc termbin.com 9999"

cal() {
    ncal -bM ${1:--3}
}

htags() {
    hasktags -e $(find `pwd` -iname '*.hs' -and \( -not -path '*.stack-work*' \) -and \( -not -name 'Setup.hs' \) -and -type f)
}

alias tldr="TLDR_COLOR_BLANK=white TLDR_COLOR_EXAMPLE=green TLDR_COLOR_COMMAND=red TLDR_COLOR_NAME=cyan TLDR_COLOR_DESCRIPTION=white tldr"

xcd() {
    cd `/usr/bin/xd $*`
}

gostack() {
    env PATH=$(stack path --compiler-tools-bin):$PATH debian_chroot=stacked bash
}

# build-deps cleanup
# http://www.webupd8.org/2010/10/undo-apt-get-build-dep-remove-build.html
aptitude-remove-dep() {
    sudo aptitude markauto $(apt-cache showsrc "$1" | grep Build-Depends | perl -p -e 's/(?:[\[(].+?[\])]|Build-Depends:|,|\|)//g');
}

visit_efs() {
    if [[ -z "$1" ]]; then
        echo "Usage: cmd src [dest]"
    else
        if [[ -z "$2" ]]; then
            local dest="/tmp/decrypted"
        else
            local dest="$2"
        fi
        if [[ ! -d "$dest" ]]; then
            mkdir $dest
        fi
        encfs "$1" $dest && ranger $dest && fusermount -u $dest
    fi
}

radio() {
    mpv --no-video --audio-display=no --playlist=$HOME/Dropbox/tux_cfg/mpd_playlists/"`ls -1 $HOME/Dropbox/tux_cfg/mpd_playlists/ | percol`"
}

ghcidf () {
    if [[ -z "$1" ]]; then
        echo "Usage: ghcidf <file.[l]hs> [<ghcid-flags>]"
    else
       ghcid $2 $3 $4 $5 -c "stack exec -- ghci" --test "main" $1
    fi
}

