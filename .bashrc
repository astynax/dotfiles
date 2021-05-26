# stop on errors
#set -euo pipefail

set +o histexpand

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# append to the history file, don't overwrite it
shopt -s histappend

# don't put duplicate lines in the history. See bash(1) for more options
# ... or force ignoredups and ignorespace
HISTCONTROL=ignoreboth:erasedups

# for setting history length see HISTSIZE and HISTFILESIZE in bash(1)
HISTSIZE=1000
HISTFILESIZE=2000

# add cmd to history immediately after execution
PROMPT_COMMAND="history -a;history -n"

# don`t add to history matching cmds
HISTIGNORE="&:bg:fg:exit:history:ranger:r:clear:encfs*:visit_efs*:tra *:yadm *"

export EDITOR=editor
export LESS="WR"

# TERM hack for urxvt
case "$TERM" in
    rxvt-unicode-256color)
        TERM=xterm-256color
        ;;
esac

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

source "$HOME/.bash.d/prelude.bash"

# aliases
_try_to_source "$HOME/.bash_aliases"

# make less more friendly for non-text input files, see lesspipe(1)
if [[ -x /usr/bin/lesspipe ]]; then
    eval "$(SHELL=/bin/sh lesspipe)"
fi

# fancy prompt
if $(which starship > /dev/null); then
    function _set_win_title() {
	echo -ne "\033]0;[$(basename $(dirname $(pwd)))/$(basename $(pwd))]\007";
    }
    export starship_precmd_user_func="_set_win_title"
    eval "$(starship init bash)"
fi

# configure dir colors
if [[ -x /usr/bin/dircolors ]]; then
    test -r ~/.dircolors \
        && eval "$(dircolors -b ~/.dircolors)" \
            || eval "$(dircolors -b)"
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

# load modules (kinda)
source "$HOME/.bash.d/utils.bash"
source "$HOME/.bash.d/efs.bash"
source "$HOME/.bash.d/ok.bash"
source "$HOME/.bash.d/fzf.bash"
source "$HOME/.bash.d/j.bash"
