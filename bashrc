# -*-mode: shell-script-*

command fortune -a

# prompt
PS_ORIG=${debian_chroot:+($debian_chroot)}\[\033[01;32m\]\u@\h\[\033[01;34m\] \w \$\[\033[00m\]
source git-prompt.sh
set_prompt () {
    last_cmd=$?
    c_blue='\[\e[34m\]'
    c_white='\[\e[37m\]'
    c_red='\[\e[31m\]'
    c_green='\[\e[32m\]'
    c_reset='\[\e[00m\]'
    s_cross='\342\234\227'
    s_check='\342\234\223'

    PS1=""
    # green checkmark for last command success, red cross for no
    if [[ $last_cmd == 0 ]]; then
        PS1+="$c_green$s_check "
    else
        PS1+="$c_red$s_cross "
    fi

    # working dir
    PS1+="$c_blue\\w "

    #end
    PS1+="\$$c_reset"
}

PROMPT_COMMAND='set_prompt'
