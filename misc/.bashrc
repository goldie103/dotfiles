# -*-mode: shell-script-*
cwhite='\[\e[37m\]'
ccyan='\[\e[36m\]'
cpurple='\[\e[35m\]'
cblue='\[\e[34m\]'
cyellow='\[\e[33m\]'
cgreen='\[\e[32m\]'
cred='\[\e[31m\]'
cblack='\[\e[30m\]'
creset='\[\e[00m\]'

# Display fortune message on shell start
# Grabs only part of defined codes to prevent excess \[\] appearing in shell.
echo -e ${cyellow:2:6}$(fortune -a)

# * prompt
set_prompt () {
  # exit status of last command
  if [[ $? == 0 ]]; then PS1=$cgreen$scheck; else PS1=\ $cred$scross; fi
  # working dir
  PS1+=\ $cblue\\w

  # git status
  source ~/dotfiles/.resources/git-prompt.sh
  cgit=$ccyan
  GIT_PS1_SHOWDIRTYSTATE=1
  GIT_PS1_SHOWSTASHSTATE=1
  GIT_PS1_SHOWUNTRACKEDFILES=1
  GIT_PS1_SYMBOLDIRTYSTAGED=$cgreen"σ"$cgit
  GIT_PS1_SYMBOLDIRTYMODIFIED=$cred"μ"$cgit
  GIT_PS1_SYMBOLUNTRACKED=$cyellow"θ"$cgit
  GIT_PS1_SYMBOLSTASHED=$cpurple"Σ"$cgit
  GIT_PS1_SYMBOLUPSTREAMDIVERGED=$cwhite"δ"$cgit
  PS1+=$cgit$(__git_ps1)

  # cleanup; add end character and reset colour
  PS1+=$ccyan" \$ "$creset
}

PROMPT_COMMAND='set_prompt'

# * aliases
alias ls="ls -h --color"        # colors and human-readable sizes by default
alias ll="ls -lv --group-directories-first" # dirs first + alphanumeric sorting
alias la="ll -A"                            # hidden files

# * automatically run in background
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
