#!/bin/bash
# Automates symlinking for each file in directory
CMD="ln -sv "

link () {
    local GLOBIGNORE=null       # so dotted files are glob-expanded
    local EXCLUDED="vimrc .gitignore link.sh .git-prompt.sh .bashrc .old"
    local OLD_DIR=".old"                                  # for existing files
    local dest_dir=${2:-$HOME}  # destination for links, $HOME is default

    local is_valid
    local dest
    local dest_file
    local src
    # iterate through the given directory
    for i in $1; do
        file=$(basename $i)
        is_valid=true
        for exclude in $EXCLUDED; do
            [[ $exclude == $file ]] && is_valid=false && break
        done

        if $is_valid && [[ ! -d $i ]]; then
            dest=$dest_dir/$file
            src=${USER_DIR:-/media/user}/dev/dotfiles/${i:2}
            # strip leading ./ and use default if $USER_DIR is unset
            # deal with existing files
            if [[ $dest -ef $src ]]; then
                # TODO how to check where a symbolic link leads
                break           # skip if the link has already been made
            elif [[ -f $dest ]]; then
                # move existing files to $OLD_DIR
                mkdir -p $OLD_DIR && mv -biv $dest $OLD_DIR/
            elif [[ -h $dest ]]; then
                rm $dest        # remove existing links
            fi

            $CMD "$src" "$dest"
        fi
    done
}

link "./*"
link "./unix/*"
link "./unix/.config/*" "$HOME/.config"
link "./emacs/*" "$HOME/.emacs.d"

$CMD "./emacs/elisp" "$HOME/.emacs.d/my-elisp"
