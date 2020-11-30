cal () {
    ncal -bM ${1:--3}
}

goprivoxy () {
    env HTTPS_PROXY=127.0.0.1:8118 \
        debian_chroot=$debian_chroot${debian_chroot:+,}proxied \
        bash
}

mcd () {
    mkdir -p -- "$*" && builtin cd -- "$*"
}

timestamp () {
    date +%Y%m%d_%H%M%S;
}

gotempdir () {
    local DIR=/tmp/$(timestamp)
    if [[ ! -a "$DIR" ]]; then
        read -n 1 -p "Folder \"$DIR\" will be created..."
        mkdir -p -- "$DIR" && builtin cd -- "$DIR"
    fi
}
