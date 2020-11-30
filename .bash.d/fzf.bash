source "$HOME/.bash.d/prelude.bash"

if [[ ! -v FZF_DIR && -v ASDF_DIR ]]; then
    export FZF_DIR="$(asdf where fzf 2>/dev/null || true)"
    if [[ ! -z "$FZF_DIR" ]]; then
        _try_to_source "$FZF_DIR/shell/completion.bash"
        _try_to_source "$FZF_DIR/shell/key-bindings.bash"
        if $(which fd > /dev/null); then
            _fzf_compgen_path() {
               fd --hidden --follow --exclude ".git" . "$1"
            }

            _fzf_compgen_dir() {
               fd --type d --hidden --follow --exclude ".git" . "$1"
            }

            export FZF_DEFAULT_COMMAND='fd --type f'
        fi
    fi
fi
