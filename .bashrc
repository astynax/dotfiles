# If not running interactively, don't do anything
if [[ -z "$PS1--" ]]; then
    exit
fi

set +o histexpand

# stop on errors
#set -euo pipefail

export EDITOR=editor
export LESS="WR"

# TERM hack
case "$TERM" in
    rxvt-unicode-256color)
        TERM=xterm-256color
        ;;
esac

# Alias definitions.
if [[ -f ~/.bash_aliases ]]; then
    . ~/.bash_aliases
fi

# completions
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
        unset a
    fi;
done
unset p
unset _paths

# don't put duplicate lines in the history. See bash(1) for more options
# ... or force ignoredups and ignorespace
HISTCONTROL=ignoreboth:erasedups

# append to the history file, don't overwrite it
shopt -s histappend

# for setting history length see HISTSIZE and HISTFILESIZE in bash(1)
HISTSIZE=1000
HISTFILESIZE=2000

# add cmd to history immediately after execution
PROMPT_COMMAND="history -a;history -n"

# don`t add to history matching cmds
HISTIGNORE="&:bg:fg:exit:history:ranger:r:clear:encfs*:visit_efs*:tra *:yadm *"

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

_try_to_source() {
    if [[ -s "$1" ]]; then
        source "$1"
    fi
}

# make less more friendly for non-text input files, see lesspipe(1)
if [[ -x /usr/bin/lesspipe ]]; then
    eval "$(SHELL=/bin/sh lesspipe)"
fi

# fancy prompt
if $(which starship > /dev/null); then
    eval "$(starship init bash)"
fi

# configure dir colors
if [[ -x /usr/bin/dircolors ]]; then
    test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
fi

# pyenv
if [[ ! -v PYENV_ROOT ]]; then
    export PYENV_ROOT="$HOME/.pyenv"
    if command -v pyenv 1>/dev/null 2>&1; then
        eval "$(pyenv init -)"
    fi
fi

# Wasmer
if [[ ! -v WASMER_DIR ]]; then
    export WASMER_DIR="$HOME/.wasmer"
    _try_to_source "$WASMER_DIR/wasmer.sh"
fi

# asdf
if [[ ! -v ASDF_DIR ]]; then
    _try_to_source "$HOME/.asdf/asdf.sh"
fi

# .ok (https://github.com/secretGeek/ok-bash)
if ! declare -f ok > /dev/null; then
    if [[ ! -v _OK__PATH_TO_ME && -d "$HOME/.ok-bash" ]]; then
        export _OK__PATH_TO_ME="$HOME/.ok-bash"
        export HISTIGNORE="$HISTIGNORE:ok*"
        source "$_OK__PATH_TO_ME/ok.sh"
    fi
fi

# fzf
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
