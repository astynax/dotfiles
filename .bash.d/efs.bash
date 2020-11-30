visit_efs () {
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
