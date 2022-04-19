if [[ -f /etc/bash_completion ]] && ! shopt -oq posix; then
    source /etc/bash_completion
fi
_paths=(
    # "/etc/bash_completion.d"  # buggy stuff!
    "$HOME/.local/etc/.bash_completion.d"
    "$HOME/.nix-profile/share/bash-completion/completions"
    "$HOME/.bash_completion.d"
)
for p in "${_paths[@]}"; do
    if [[ -d "$p" ]]; then
        for a in $p/*; do
            source "$a"
        done
    fi;
done
unset a
unset p
unset _paths
