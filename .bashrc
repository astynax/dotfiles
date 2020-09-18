# If not running interactively, don't do anything
if [[ -z "$PS1--" ]]; then
    exit
fi

set +o histexpand

# stop on errors
#set -euo pipefail

export EDITOR=editor
export LESS="WR"
export LV="-c"

# user completions
if [[ -d "$HOME/.bash_completion.d" ]]; then
    for a in $HOME/.bash_completion.d/*; do
        source "$a"
    done
    unset a
fi

if [[ -d "$HOME/.local/etc/bash_completion.d" ]]; then
    for a in $HOME/.local/etc/bash_completion.d/*; do
        source "$a"
    done
    unset a
fi

# Alias definitions.
if [[ -f ~/.bash_aliases ]]; then
    . ~/.bash_aliases
fi

# enable programmable completion
if [[ -f /etc/bash_completion ]] && ! shopt -oq posix; then
    . /etc/bash_completion
fi

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
HISTIGNORE="&:bg:fg:exit:history:ranger:r:clear:encfs*:visit_efs*:tra *"

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# make less more friendly for non-text input files, see lesspipe(1)
if [[ -x /usr/bin/lesspipe ]]; then
    eval "$(SHELL=/bin/sh lesspipe)"
fi

# fancy prompt
if $(which starship > /dev/null); then
    if $(which starship > /dev/null); then
        eval "$(starship init bash)"
    fi
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
    if [[ -s "$WASMER_DIR/wasmer.sh" ]]; then
        source "$WASMER_DIR/wasmer.sh"
    fi
fi

# asdf
if [[ ! -v ASDF_DIR ]]; then
    if [[ -s "$HOME/.asdf/asdf.sh" ]]; then
        source "$HOME/.asdf/asdf.sh"
    fi
fi

# .ok (https://github.com/secretGeek/ok-bash)
if ! declare -f ok > /dev/null; then
    if [[ ! -v _OK__PATH_TO_ME ]]; then
        export _OK__PATH_TO_ME="$HOME/.ok-bash"
        export HISTIGNORE="$HISTIGNORE:ok*"
    fi
    if [[ -s "$_OK__PATH_TO_ME/ok.sh" ]]; then
        source "$_OK__PATH_TO_ME/ok.sh"
    fi
fi

