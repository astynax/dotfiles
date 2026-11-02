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

if [[ -d "$HOME/.cargo/env" ]]; then
    source "$HOME/.cargo/env"
fi

if [[ $- == *i* ]]; then
    if type brew &>/dev/null; then
        FPATH="$(brew --prefix)/share/zsh/site-functions:${FPATH}"
    fi
    export FPATH="$FPATH:$HOME/.local/share/zsh/site-functions"
    export FPATH="$FPATH:/opt/local/share/zsh/site-functions"

    autoload -Uz compinit && compinit

    zstyle ':completion:*' menu select

    if [[ "$TERM" != "dumb" ]]; then
        source <(/usr/local/bin/starship init zsh --print-full-init)
    else
        export PS1="%d %# "
    fi

    if type -p "fzf" > /dev/null; then
        export FZF_DEFAULT_OPTS_FILE="$HOME/.config/.fzfrc"
        source "$HOME/.local/share/zsh/site-functions/_fzf"
    fi

    source ~/.orbstack/shell/init.zsh 2>/dev/null || true

    alias ll="eza --icons -l"
    alias ls=eza
    alias tree="eza --tree --git-ignore"
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
