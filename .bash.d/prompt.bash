if $(which starship > /dev/null); then
    function _set_win_title() {
        local here here_dir parent dir
        here=$(pwd)
        here_dir=$(dirname "$here")
        parent=$(basename "$here_dir")
        dir=$(basename "$here")
        echo -ne "\033]0;[$parent/$dir]\007"
    }
    export starship_precmd_user_func="_set_win_title"
    eval "$(starship init bash)"
fi
