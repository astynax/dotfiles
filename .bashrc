# If not running interactively, don't do anything
if [[ -z "$PS1--" ]]; then
    exit
fi

set +o histexpand

# stop on errors
#set -euo pipefail

# split words on this chars only (useful for ``for``)
#IFS=$'\n\t'

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
HISTIGNORE="&:bg:fg:exit:history:ranger:r:clear:encfs*:visit_efs*"

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# make less more friendly for non-text input files, see lesspipe(1)
if [[ -x /usr/bin/lesspipe ]]; then
    eval "$(SHELL=/bin/sh lesspipe)"
fi

# set a fancy prompt (non-color, unless we know we "want" color)
if [[ -z "$MINIMAL_PROMPT" ]]; then
    case "$TERM" in
        xterm-color) color_prompt=yes;;
        rxvt-unicode-256color) color_prompt=yes;;
    esac

    if [[ -z "$color_prompt" ]]; then
        if [[ -x /usr/bin/tput ]] && tput setaf 1 >&/dev/null; then
            color_prompt=yes
        else
            color_prompt=
        fi
    fi

    if [[ -z "$debian_chroot" ]] && [[ -r /etc/debian_chroot ]]; then
        debian_chroot=$(cat /etc/debian_chroot)
    fi

    if [[ "$color_prompt" = yes ]]; then
        if [[ -f ~/.bash_prompt ]]; then
            source ~/.bash_prompt
        else
            PS1='${debian_chroot:+($debian_chroot)}\[\033[04;34m\]\u@\h\[\033[00m\]:\[\033[02;33m\]\w\[\033[00m\]\$ '
        fi
    else
        PS1='${debian_chroot:+($debian_chroot)}\u@\h:\w\$ '
    fi
    unset color_prompt
else
    PS1='${debian_chroot:+($debian_chroot) }\w\$ '
fi

# configure dir colors
if [[ -x /usr/bin/dircolors ]]; then
    test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
fi

# Nix
if [[ ! -v NIX_PROFILE ]]; then
    export NIX_PROFILE="$HOME/.nix-profile/etc/profile.d/nix.sh"
    if [[ -s "$NIX_PROFILE" ]]; then
        . "$NIX_PROFILE"
    fi
fi

# SDKman
if [[ ! -v SDKMAN_DIR ]]; then
    export SDKMAN_DIR="$HOME/.sdkman"
    if [[ -s "$SDKMAN_DIR/bin/sdkman-init.sh" ]]; then
        . "$SDKMAN_DIR/bin/sdkman-init.sh"
    fi
fi

# pyenv
if [[ ! -v PYENV_ROOT ]]; then
    export PYENV_ROOT="$HOME/.pyenv"
    if command -v pyenv 1>/dev/null 2>&1; then
        eval "$(pyenv init -)"
    fi
fi

# rbenv
if [[ ! -v RBENV_ROOT ]]; then
    export RBENV_ROOT="$HOME/.rbenv"
    if command -v rbenv 1>/dev/null 2>&1; then
        eval "$(rbenv init -)"
    fi
fi

# Wasmer
if [[ ! -v WASMER_DIR ]]; then
    export WASMER_DIR="$HOME/.wasmer"
    if [[ -s "$WASMER_DIR/wasmer.sh" ]]; then
        source "$WASMER_DIR/wasmer.sh"
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

# commacd (https://github.com/shyiko/commacd)
if ! declare -f , > /dev/null && [[ -s $HOME/.commacd.sh ]]; then
    export HISTIGNORE="$HISTIGNORE:,*"
    source $HOME/.commacd.sh
fi

