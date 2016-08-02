#!/bin/zsh

f="-i"
v=""
while getopts "vif" opt; do
  case $opt in
    v) v="-v";;
    f) f="-f";;
    \?) echo "Invalid option: -$OPTARG" >&2 && exit 1;;
  esac
done

d () { mkdir -p $v "$1" }
l () { d "$1" && ln -s $v $f "$DOT/$2" "$1/${3:-$2}" }

source ./env && l $HOME env .zshenv

vimp_plugin () {
  if [[ ! -e $1 ]]; then
    [[ ! -d $VIMPERATOR_RUNTIME ]] && d $VIMPERATOR_RUNTIME
    curl -o "$VIMPERATOR_RUNTIME/${2:-$1}" "https://raw.githubusercontent.com/vimpr/vimperator-plugins/master/$1"
  fi
}

init_zsh () {
  l $1 init.zsh .zshrc
  l $1 profile .zprofile
  l $1 zsh custom
  local dir="$1/oh-my-zsh"
  l $dir/themes my.zsh-theme
  [[ ! -d $dir ]] && git clone git://github.com/robbyrussell/oh-my-zsh.git "$dir"
}
init_vim () {
  l $1 init.vim
  local vundlepath=$1/bundle/Vundle.vim
  if [[ ! -e $vundlepath ]]; then
    git clone https://github.com/VundleVim/Vundle.vim.git "$vundlepath"
    ${2:-vim} +PluginInstall +qall
  fi
}
init_emacs () {
  l $1 init.el
  l $1 my-elisp
}
init_vimp () {
  l $1 vimperatorrc.vimp init.vimp
  vimp_plugin "_smooziee.js" "smooziee.js"
  vimp_plugin "stylish.js"
}


l $HOME profile .bash_profile
l $XDG_CONFIG_HOME/X11 Xresources
l $HOME xinitrc .xinitrc
init_emacs $HOME/.emacs.d
init_vim $XDG_CONFIG_HOME/nvim nvim
init_vimp $VIMPERATOR_RUNTIME
init_zsh $ZDOTDIR
