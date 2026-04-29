setopt append_history
setopt hist_ignore_dups
setopt hist_ignore_all_dups
setopt hist_ignore_space
setopt hist_save_no_dups
setopt hist_expire_dups_first
setopt hist_reduce_blanks

setopt emacs
export EDITOR=mg
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
    export FPATH="$FPATH:$HOME/.local/share/zsh/site-functions"

    autoload -Uz compinit && compinit

    zstyle ':completion:*' menu select

    source <(/usr/local/bin/starship init zsh --print-full-init)

    if type -p "fzf" > /dev/null; then
        export FZF_DEFAULT_OPTS_FILE="$HOME/.config/.fzfrc"
        source "$HOME/.local/share/zsh/site-functions/_fzf"
    fi

    if type -p "poe" > /dev/null; then
        autoload -Uz _poe
        compdef _poe poe
    fi

    if type -p "but" > /dev/null; then
        autoload -Uz _but
        compdef _but but
    fi

    alias ll="eza --icons -l"
    alias ls=eza
    alias et="eza --tree --git-ignore"
    alias cat=bat
    alias r=ranger
    alias cal="ncal -3"

    alias e=emacs
    alias enw="emacs -nw"
    alias ec="emacsclient -nc"

    alias nano=mg

    function timestamp () {
        date +%Y%m%d_%H%M%S;
    }

    autoload -Uz mcd
    autoload -Uz gotempdir
    autoload -Uz renameme
    autoload -Uz p
    autoload -Uz uv-try

    function wttr() {
        curl 'wttr.in/yerevan?T&lang=en'
    }

    function goghcup () {
        export DEBIAN_CHROOT=ghcup
        export PATH=$HOME/.ghcup/bin:$PATH
    }

    export TLDR_AUTO_UPDATE_DISABLED=yes
    alias tldr="tldr --platform=osx"

    export PAGER="less -r"
fi
