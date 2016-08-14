# zsh renaming
autoload -U zmv
# completion
zmodload zsh/complist
autoload -U compinit && compinit
# no auto complete
unsetopt correct


#ZSH_THEME="agnoster"
ZSH_THEME="my"

# _ and - are interchangeable
HYPHEN_INSENSITIVE="true"
# enable command autocorrect
ENABLE_CORRECTION="true"
# display red dots while waiting for completion
COMPLETION_WAITING_DOTS="true"

plugins=(git)

source $ZSH/oh-my-zsh.sh
# tell me where to install a command if entering an unknown command
source /usr/share/doc/pkgfile/command-not-found.zsh

