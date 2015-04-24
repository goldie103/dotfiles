# -*-mode: shell-script-*

command fortune -a

# prompt
PS_ORIG=${debian_chroot:+($debian_chroot)}\[\033[01;32m\]\u@\h\[\033[01;34m\] \w \$\[\033[00m\]
set_prompt () {
    last_cmd=$?
    C_BLUE='\[\e[01;34m\]'
    C_WHITE='\[\e[01;37m\]'
    C_RED='\[\e[01;31m\]'
    C_GREEN='\[\e[01;32m\]'
    C_RESET='\[\e[01;00m\]'
    S_CROSS='\342\234\227'
    S_CHECK='\342\234\223'

    PS1=""
    
    # green checkmark for last command success, red cross for no
    if [[ $last_cmd == 0 ]]; then
        PS1+="$C_GREEN$S_CHECK"
    else
        PS1+=" $C_RED$S_CROSS"
    fi

    # working dir
    PS1+=" $C_BLUE\\w"

    # git status
    source .git-prompt.sh
    GIT_PS1_SHOWDIRTYSTATE=1
    GIT_PS1_SHOWSTASHSTATE=1
    GIT_PS1_SHOWUNTRACKEDFILES=1
    GIT_PS1_SHOWCOLORHINTS=1
    GIT_PS1_SHOWUPSTREAM="auto"
    GIT_PS1_SYMBOLDIRTYSTAGED=$S_CHECK
    GIT_PS1_SYMBOLDIRTYMODIFIED=$S_CROSS
    GIT_PS1_SYMBOLUNTRACKED=$S_CROSS
    GIT_PS1_SYMBOLSTASHED="S"
    GIT_PS1_SYMBOLUPSTREAMDIVERGED="-"
    
    PS1+="__git_ps1"

    #end
    PS1+=" \$ $C_RESET"
}

PROMPT_COMMAND='set_prompt'
