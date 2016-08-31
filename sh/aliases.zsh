
# colors
alias grep='grep --color'
alias ls='ls --color'

# quick things
alias l='ls -A'           # almost all
alias ll='ls -lFha'       # size, type, human readable
alias mmv='noglob zmv -W' # no need to quote globs

# safety
alias rm='rm -ri'
alias cp='cp -ri'
alias mv='mv -i'

# files
# open with xdg-open
alias o='xdg-open'
alias -s pdf='o'
alias -s jpg='o'
alias -s png='o'
alias -s xoj='xournal'	# xournal doesn't work with xdg-open for some reason
# list contents of packed files
alias -s zip='unzip -l'
alias -s rar='unrar l'
alias -s tar='tar tf'
alias -s tar.gz='echo '

# directories
hash -d -- DOW=/home/kelly/dow
hash -d -- PIC=/home/kelly/pic
hash -d -- DOC=/home/kelly/doc
hash -d -- DOT=/home/kelly/dotfiles

# arch pacman aliases
alias i='pacaur -S'

# editor
alias e="$EDITOR"
alias ed="$VISUAL"
