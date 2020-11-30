# .ok (https://github.com/secretGeek/ok-bash)
if ! declare -f ok > /dev/null; then
    if [[ ! -v _OK__PATH_TO_ME && -d "$HOME/.ok-bash" ]]; then
        export _OK__PATH_TO_ME="$HOME/.ok-bash"
        export HISTIGNORE="$HISTIGNORE:ok:ok *"
        source "$_OK__PATH_TO_ME/ok.sh"
    fi
fi
