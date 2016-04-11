#!/bin/sh

source env

exists () { command -v $1 >/dev/null 2>&1; }

[[ "$EUID" -ne 0 ]] && SUDO=true || SUDO=false

# install git
if [[ ! "$(exists git)" ]]; then
	if ! $SUDO; echo "Please install git" && exit; fi
	if [ ! "$(exists apt)" ]; then apt-get install git
	elif [[ ! "$(exists pacman)" ]]; then pacman -S git
	fi
fi

# vim
VIMLOC=${VIMDOTDIR:-$XDG_CONFIG_HOME/vim}
[[ ! -d "$VIMLOC" ]] && mkdir $VIMLOC && ln -s "$DOT/init.vim" "$VIMLOC/vimrc"

# vundle
[[ ! -d "$VIMLOC/vundle" ]] &&
	git clone https://github.com/VundleVim/Vundle.vim.git $VIMLOC/bundle/Vundle.vim &&
	vim +PluginInstall +qall

# vimperator
[[ ! -d "$XDG_CONFIG_HOME/vimperator/plugin" ]] &&
	mkdir "$XDG_CONFIG_HOME/vimperator/plugin" &&
	curl -o "$XDG_CONFIG_HOME/vimperator/plugin/smooziee.js" https://raw.githubusercontent.com/vimpr/vimperator-plugins/master/_smooziee.js
