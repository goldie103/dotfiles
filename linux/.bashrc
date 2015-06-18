# -*-mode: shell-script-*

# Display yellow fortune message on shell start
echo -e '\e[33m'$(fortune -a)

# * prompt
set_prompt () {
  local cwhite='\[\e[37m\]'
  local ccyan='\[\e[36m\]'
  local cpurple='\[\e[35m\]'
  local cblue='\[\e[34m\]'
  local cyellow='\[\e[33m\]'
  local cgreen='\[\e[32m\]'
  local cred='\[\e[31m\]'
  local cblack='\[\e[30m\]'
  local creset='\[\e[00m\]'

  # exit status of last command
  if [[ $? == 0 ]]; then PS1=$cgreen$scheck; else PS1=$cred$scross; fi

  # working dir
  PS1+=\ $cblue\\w

  # git status
  source ~/dotfiles/.resources/git-prompt.sh
  cgit=$ccyan
  GIT_PS1_SHOWDIRTYSTATE=1
  GIT_PS1_SHOWSTASHSTATE=1
  GIT_PS1_SHOWUNTRACKEDFILES=1
  GIT_PS1_SHOWUPSTREAM="auto verbose"
  PS1+=$cgit$(__git_ps1 " (%s"$cgit")")

  # cleanup; add end character and reset colour
  PS1+=$ccyan" \$ "$creset
}

PROMPT_COMMAND='set_prompt'

# * aliases
alias ls="ls -Ah --color"        # all files, colors and human-readable sizes
alias gits="git status -s"
alias gita="git add"
alias gitc="git commit"

# always colors
alias grep="grep --color=always"

# less with color code support
alias less="less -R"
# less is more
alias more=less

alias apti="apt install"
function apts() {
  apt search "$@" | less -R
}

# point vim to custom vimrc
export VIMINIT='source $MYVIMRC'
export MYVIMRC='/media/kelly/User/.vim/vimrc'

# automatically run in background
function firefox() { command firefox "$@" & }
function emacs() { command emacs "$@" & }

# * functions
# identify archive type and extract
extract () {
  if [ -f $1 ] ; then
      case $1 in
        *.tar.bz2)        tar xjf $1        ;;
        *.tar.gz)         tar xzf $1        ;;
        *.bz2)            bunzip2 $1        ;;
        *.rar)            unrar x $1        ;;
        *.gz)             gunzip $1         ;;
        *.tar)            tar xf $1         ;;
        *.tbz2)           tar xjf $1        ;;
        *.tgz)            tar xzf $1        ;;
        *.zip)            unzip $1          ;;
        *.Z)              uncompress $1     ;;
        *)                echo "'$1' cannot be extracted via extract()" ;;
      esac
  else
    echo "'$1' is not a valid file"
  fi
}

# Local Variables:
# outline-regexp: " *# ? \\*\\{1,8\\}"
# End:
