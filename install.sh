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
if [[ ! -d $VIMPERATOR_RUNTIME/plugin ]]; then
	mkdir "$VIMPERATOR_RUNTIME/plugin" &&
	curl -o "$VIMPERATOR_RUNTIME/plugin/smooziee.js" https://raw.githubusercontent.com/vimpr/vimperator-plugins/master/_smooziee.js
fi
