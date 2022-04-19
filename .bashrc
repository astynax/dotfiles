# stop on errors
#set -euo pipefail

set +o histexpand
set -o noclobber

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
HISTIGNORE="&:bg:fg:exit:history:ranger:r:clear:encfs*:visit_efs*"

export EDITOR=editor
export LESS="WR"

# TERM hack for urxvt
case "$TERM" in
    rxvt-unicode-256color)
        TERM=xterm-256color
        ;;
esac

# make less more friendly for non-text input files, see lesspipe(1)
if [[ -x /usr/bin/lesspipe ]]; then
    eval "$(SHELL=/bin/sh lesspipe)"
fi

[[ -s "$HOME/.bash_aliases" ]] && source "$HOME/.bash_aliases"

source "$HOME/.bash.d/completion.bash"
source "$HOME/.bash.d/prompt.bash"
source "$HOME/.bash.d/dircolors.bash"
source "$HOME/.bash.d/utils.bash"
source "$HOME/.bash.d/efs.bash"
source "$HOME/.bash.d/ok.bash"
source "$HOME/.bash.d/j.bash"
