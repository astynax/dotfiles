if type brew &>/dev/null; then
  FPATH="$(brew --prefix)/share/zsh/site-functions:${FPATH}"
fi

export NVM_DIR="$HOME/.nvm"
[ -s "/usr/local/opt/nvm/nvm.sh" ] && \
    . "/usr/local/opt/nvm/nvm.sh"  # This loads nvm
[ -s "/usr/local/opt/nvm/etc/bash_completion.d/nvm" ] && \
    . "/usr/local/opt/nvm/etc/bash_completion.d/nvm"

export DICTIONARY=russian-aot,en_US

export LANGUAGE=en_US.UTF-8
export LANG=en_US.UTF-8

if locale -a | grep ru_RU.UTF-8 > /dev/null; then
    export LC_TIME=ru_RU.UTF-8
    export LC_NUMERIC=ru_RU.UTF-8
    export LC_COLLATE=ru_RU.UTF-8
    export LC_CTYPE=ru_RU.UTF-8
    export LC_MEASUREMENT=ru_RU.UTF-8
fi

export PATH=$PATH:$HOME/.local/bin

if [[ $- == *i* ]]; then
    source <(/usr/local/bin/starship init zsh --print-full-init)

    autoload -Uz compinit
    compinit

    alias ll="eza -l"
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
fi
