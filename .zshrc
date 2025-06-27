setopt append_history
setopt hist_ignore_dups
setopt hist_ignore_space
setopt hist_save_no_dups

setopt emacs
export FCEDIT=mg

if type brew &>/dev/null; then
    FPATH="$(brew --prefix)/share/zsh/site-functions:${FPATH}"
fi

export NVM_DIR="$HOME/.nvm"
[ -s "/usr/local/opt/nvm/nvm.sh" ] && \
    . "/usr/local/opt/nvm/nvm.sh"  # This loads nvm
[ -s "/usr/local/opt/nvm/etc/bash_completion.d/nvm" ] && \
    . "/usr/local/opt/nvm/etc/bash_completion.d/nvm"

if [[ -d "$HOME/.cargo/env" ]]; then
    source "$HOME/.cargo/env"
fi

if [[ $- == *i* ]]; then
    autoload -Uz compinit
    compinit

    source <(/usr/local/bin/starship init zsh --print-full-init)
    source <(fzf --zsh) || true

    [[ $(type -p "poe") ]] && source <(poe _zsh_completion) && compdef _poe poe

    alias ll="eza --icons -l"
    alias ls=eza
    alias cat=bat
    alias r=ranger

    alias e=emacs
    alias enw=emacs -nw

    function cal() {
        ncal -3
    }

    function wttr() {
        curl 'wttr.in/yerevan?T&lang=en'
    }

    function timestamp () {
        date +%Y%m%d_%H%M%S;
    }

    function gotempdir () {
        local SUFFIX
        local DIR=/tmp/$(timestamp)
        read 'SUFFIX?You can add a suffix if you want: '
        if [[ ! -z "$SUFFIX" ]]; then
            DIR=$DIR'_'$SUFFIX
        fi
        if [[ ! -a "$DIR" ]]; then
            echo "Folder \"$DIR\" will be created..."
            read -n 1
            mkdir -p -- "$DIR" && builtin cd -- "$DIR"
        fi
    }

    function goghcup () {
        export DEBIAN_CHROOT=ghcup
        export PATH=$HOME/.ghcup/bin:$PATH
    }

    if [[ ! "$SHLVL" -ge 2 ]]; then
        fortune || true
    fi
fi
