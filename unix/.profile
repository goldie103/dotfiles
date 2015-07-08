# Find user directory
USER_DIR="/media/user"
if [[ ! -d $USER_DIR ]]; then
  echo "Can't find user dir, using $HOME."
  USER_DIR=$HOME
fi

# * settings
shopt -s nocaseglob             # case insensitive globbing
shopt -s cdspell                # auto correct path name typos when using cd
shopt -s nullglob

# * exports
export XDG_CONFIG_HOME="${USER_DIR:-$HOME}/.config"
export USER_DIR=$USER_DIR
export ALTERNATE_EDITOR=""
export EDITOR="emacsclient -t"
export VISUAL="emacsclient -a ''"

# path
[[ -d "$HOME/bin" ]] && PATH="$HOME/bin:$PATH" # include private bin if exists
export PATH="$PATH:~/.local/bin"               # local application bin
export PATH="/usr/local/heroku/bin:$PATH"      # added by Heroku Toolbelt

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
