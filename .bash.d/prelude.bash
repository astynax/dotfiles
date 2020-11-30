_try_to_source() {
    if [[ -s "$1" ]]; then
        source "$1"
    fi
}
