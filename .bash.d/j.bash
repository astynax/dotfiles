# jumps
# source: https://www.datascienceworkshops.com/blog/quickly-navigate-your-filesystem-from-the-command-line/
export MARKPATH=$HOME/.marks

j () {
    cd -P "$MARKPATH/$1" 2>/dev/null || echo "No such mark: $1"
}

mark () {
    if [[ ! -d "$MARKPATH" ]]; then
        mkdir -p "$MARKPATH"
    fi
    ln -s "$(pwd)" "$MARKPATH/$1"
}

unmark () {
    rm -i "$MARKPATH/$1"
}

marks () {
    ls -l "$MARKPATH" | sed 's/  / /g' | cut -d' ' -f9- | sed 's/ -/\t-/g' && echo
}

_completemarks () {
    local curw=${COMP_WORDS[COMP_CWORD]}
    local wordlist=$(find $MARKPATH -type l -printf "%f\n")
    COMPREPLY=($(compgen -W '${wordlist[@]}' -- "$curw"))
    return 0
}

complete -F _completemarks j unmark
