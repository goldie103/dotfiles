# -*-mode: shell-script-*
C_BLUE='\[\e[34m\]'
C_WHITE='\[\e[37m\]'
C_RED='\[\e[31m\]'
C_GREEN='\[\e[32m\]'
C_RESET='\[\e[00m\]'
S_CROSS='\342\234\227'
S_CHECK='\342\234\223'


# Adds random coloured message at start of shell.
# Grabs only part of defined codes to prevent excess \[\] appearing in shell.
# TODO get in an indented block
echo -e "${C_RED:2:6}$(command fortune -a)"

# prompt
PS_ORIG="${debian_chroot:+($debian_chroot)}\[\033[01;32m\]\u@\h\[\033[01;34m\]
\w \$\[\033[00m\]"
set_prompt () {
    last_cmd=$?
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
    # TODO get this working
    source ~/dotfiles/.resources/git-prompt.sh
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
    PS1+="$__git_ps1"

    # cleanup; add end character and reset colour
    PS1+=" \$ $C_RESET"
}

PROMPT_COMMAND='set_prompt'
