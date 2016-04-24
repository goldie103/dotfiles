#!/bin/zsh

exists () { command -v $1 >/dev/null 2>&1; }

[[ $EUID -ne 0 ]] && SUDO=true || SUDO=false

source ./env
[[ ! -L $HOME/.zshenv ]] && ln -sv "$DOT/env" "$HOME/.zshenv"

# install git
[[ ! $(exists git) ]] && echo "Please install git"

# vim
VIMLOC=${VIMDOTDIR:-$XDG_CONFIG_HOME/vim}
if [[ ! -d $VIMLOC ]]; then
  echo "Setting up vim ..."
  mkdir $VIMLOC && ln -s "$DOT/init.vim" "$VIMLOC/vimrc"
fi

# vundle
if [[ ! -d $VIMLOC/vundle ]]; then
  echo "Setting up vundle..."
	git clone https://github.com/VundleVim/Vundle.vim.git $VIMLOC/bundle/Vundle.vim && vim +PluginInstall +qall
fi

# vimperator
init_vimperator () { [[ ! -L $1/init.vimp || $3 ]] && ln -sv $DOT/init.vimp $1/init.vimp }
init_vimperator_plugin () {
  local plugin_dir=$1/plugin
  echo "Getting smooziee.js"
  [[ ! -d $plugin_dir ]] && mkdir "$plugin_dir"
  curl -o "$plugin_dir/smooziee.js" https://raw.githubusercontent.com/vimpr/vimperator-plugins/master/_smooziee.js
}

# emacs
init_emacs () {
  if [[ ! -d $HOME/.emacs.d || $3 ]]; then
    echo "Setting up emacs..."
    mkdir "$HOME/.emacs.d"
    ln -sv $2/init.el $1/init.el
    ln -sv $2/my-elisp $1/my-elisp
  fi
}

init_emacs $HOME/.emacs.d $DOT/.emacs.d
init_vimperator $VIMPERATOR_RUNTIME
[[ ! -f $VIMPERATOR_RUNTIME
