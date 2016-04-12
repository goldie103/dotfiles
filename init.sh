#!/bin/zsh

source ./env
ln -sv $HOME/.zshenv $DOT/env

exists () { command -v $1 >/dev/null 2>&1 }

# install git
[[ ! "$(exists git)" ]] && echo "Please install git" && exit

# vim
setup_vim_vundle () {
  if [[ ! -d $1 ]]; then
    git clone https://github.com/VundleVim/Vundle.vim.git $VIMLOC/bundle/Vundle.vim
    vim +PluginInstall +qall
  fi
}
setup_vim () {
  if [[ ! -d $1 ]]; then
    mkdir -v $1
    ln -sv "$DOT/init.vim" "$1/vimrc"
  fi
  setup_vim_vundle $1
}

# vimperator
setup_vimperator_plugin () {
  if [[ ! -d $1 ]]; then
    mkdir -v "$1"
    curl -o "$1/smooziee.js" https://raw.githubusercontent.com/vimpr/vimperator-plugins/master/_smooziee.js
  fi
}
setup_vimperator () {
  if [[ ! -d $1 ]]; then
    mkdir -v "$1"
    ln -sv "$DOT/init.vimp" $1/init.vimp"
  fi
  setup_vimperator_plugin $1/plugin
}

# emacs
setup_emacs_my () {
  if [[ ! -d $1 ]]; then
    mkdir -v "$1"
    ln -sv $DOT/my-elisp $1
  fi
}
setup_emacs () {
  if [[ ! -d $1 ]]; then
    mkdir -v "$1"
    ln -sv $DOT/init.el $1/init.el
  fi
  setup_emacs_my $1/my-elisp
}


setup_vim $VIMDOTDIR/init.vim
setup_vimperator $VIMPERATOR_RUNTIME
setup_emacs $HOME/.emacs.d

