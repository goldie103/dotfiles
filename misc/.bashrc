# -*-mode: shell-script-*
cwhite='\[\e[37m\]'
cpurple='\[\e[35m\]'
cblue='\[\e[34m\]'
cyellow='\[\e[33m\]'
cgreen='\[\e[32m\]'
cred='\[\e[31m\]'
cblack='\[\e[30m\]'
creset='\[\e[00m\]'

scross='\342\234\227'
scheck='\342\234\223'

# Display fortune message on shell start
# Grabs only part of defined codes to prevent excess \[\] appearing in shell.
# TODO get in an indented block
echo -e ${cyellow:2:6}$(fortune -a)

# prompt
set_prompt () {
    last_cmd=$?
    PS1=""

    # last command status
    if [[ $last_cmd == 0 ]]; then
        PS1+=$cgreen$scheck
    else
        PS1+=" "$cred$scross
    fi

    # working dir
    PS1+=\ $cblue\\w

    # git status
    # TODO symbols
    source ~/dotfiles/.resources/git-prompt.sh
    GIT_PS1_SHOWDIRTYSTATE=1
    GIT_PS1_SHOWSTASHSTATE=1
    GIT_PS1_SHOWUNTRACKEDFILES=1
    GIT_PS1_SHOWCOLORHINTS=1
    GIT_PS1_SHOWUPSTREAM="auto"
    GIT_PS1_SYMBOLDIRTYSTAGED="$scheck"
    GIT_PS1_SYMBOLDIRTYMODIFIED="$scross"
    GIT_PS1_SYMBOLUNTRACKED="$scross"
    GIT_PS1_SYMBOLSTASHED="σ"
    GIT_PS1_SYMBOLUPSTREAMDIVERGED="-"
    PS1+=$cpurple$(__git_ps1)

    # cleanup; add end character and reset colour
    PS1+=$cgreen" \$ "$creset
}

PROMPT_COMMAND='set_prompt'
