#!/bin/zsh

source ./env && ln -svi $DOT/env $HOME/.zshenv

d () { mkdir -pv "$1" }
l () { mkdir -pv "$1" && ln -svi "$DOT/$2" "$1/${3:-$2}" }

setup_vim_vundle () {
  if [[ ! -d $1 ]]; then
    git clone https://github.com/VundleVim/Vundle.vim.git "$1/bundle/Vundle.vim"
    vim +PluginInstall +qall
  fi
}
setup_vimperator_plugin () {
  if [[ ! -f "$1/smooziee.js" ]]; then
    d $1
    curl -o "$1/smooziee.js" https://raw.githubusercontent.com/vimpr/vimperator-plugins/master/_smooziee.js
  fi
}

l $VIMDOTDIR init.vim && setup_vim_vundle $VIMDOTDIR
l $VIMPERATOR_RUNTIME init.vimp && setup_vimperator_plugin $VIMPERATOR_RUNTIME
l $HOME/.emacs.d init.el && l $HOME/.emacs.d/ my-elisp
l $ZDOTDIR init.zsh .zshrc

