# Find user directory
[[ -d $USER_DIR ]] && USER_DIR="/media/user"

# * settings
shopt -s nocaseglob             # case insensitive globbing
shopt -s cdspell                # auto correct path name typos when using cd
shopt -s nullglob

# * exports
export USER_DIR="${USER_DIR:-$HOME}"
export ALTERNATE_EDITOR=""
export EDITOR="emacsclient -t"
export VISUAL="emacsclient -c ''"

# path
add_path () {
  # add a directory to PATH if it exists
  [[ -d $1 ]] && PATH="$1:$PATH"
}

add_path "$HOME/bin"             # private bin
add_path "$HOME/.local/bin"      # local application bin
add_path "/usr/local/heroku/bin" # heroku apps

# source files
source_file () {
    # find source directory
    SRC_DIR=$USER_DIR/dev/dotfiles/unix/src
    if [[ ! -d $SRC_DIR ]]; then
        echo "Can't find source directory, using $HOME."
        SRC_DIR=$HOME
    fi

    # source file if it exists
    [[ -f $SRC_DIR/$1 ]] && source $SRC_DIR/$1
}


source_file .bashrc
source_file liquidprompt
