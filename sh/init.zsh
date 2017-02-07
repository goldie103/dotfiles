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

# Renaming
autoload -U zmv
# Completion
zmodload zsh/complist
autoload -U compinit && compinit
HYPHEN_INSENSITIVE="true"
COMPLETION_WAITING_DOTS="true"
# cd alternative
autoload -Uz chpwd_recent_dirs cdr add-zsh-hook
add-zsh-hook chpwd chpwd_recent_dirs

source /bin/virtualenvwrapper.sh
