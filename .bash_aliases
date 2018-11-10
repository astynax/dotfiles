#!/bin/bash

# some more ls aliases
alias ls='/bin/ls -Fv --color=always'
alias ll='/bin/ls -FvlGh --color=always --time-style=long-iso'
alias la='/bin/ls -FvA --color=always'
alias l='/bin/ls -af1v'

alias playlist-dl="youtube-dl -o '~/Downloads/youtube/%(uploader)s/%(playlist_mb_title)s/%(playlist_index)s. %(title)s.%(ext)s'"

alias e="emacs"
alias ec="emacsclient -nq"
alias eq="emacs -Q"
alias enq="emacs -Q -nw"

alias path='echo -e ${PATH//:/\\n}'

cal() {
    ncal -bM ${1:--3}
}

xcd() {
    cd `/usr/bin/xd $*`
}

gostack() {
    env PATH=$(stack path --compiler-tools-bin):$PATH debian_chroot=stacked bash
}

mcd () {
    mkdir -p -- "$*" ; builtin cd -- "$*" ;
}

mans () {
    man $1 | grep -iC2 --color=always $2 | less ;
}

buf () {
    local filename=$1
    local filetime=$(date +%Y%m%d_%H%M%S)
    cp -a "${filename}" "${filename}_${filetime}"
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

