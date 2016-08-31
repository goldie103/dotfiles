source $ADOTDIR/antigen.zsh
antigen use oh-my-zsh

antigen bundles <<EOBUNDLES
  pip
  colorize
  command-not-found
  zsh-users/zsh-history-substring-search
  zsh-users/zsh-syntax-highlighting
  zsh-users/zsh-autosuggestions
  zsh-users/zsh-completions
EOBUNDLES

antigen theme agnoster
antigen apply

autoload -U zmv
zmodload zsh/complist
autoload -U compinit && compinit
HYPHEN_INSENSITIVE="true"
COMPLETION_WAITING_DOTS="true"
