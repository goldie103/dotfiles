
# colors
alias grep='grep --color'
alias ls='ls --color'

# quick things
alias l='ls -A'           # almost all
alias ll='ls -lFha'       # size, type, human readable
alias mmv='noglob zmv -W' # no need to quote globs

# safety
alias rm='rm -i'
alias cp='cp -i'
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
hash -d -- DOW=/home/kelly/Downloads
hash -d -- PIC=/home/kelly/Pictures
hash -d -- DOC=/home/kelly/Documents
hash -d -- DOT=/home/kelly/dotfiles
hash -d -- W=/home/kelly/Documents/work
hash -d -- WM1=/home/kelly/Documents/work/math1001
hash -d -- WM2=/home/kelly/Documents/work/math1002
hash -d -- WI9=/home/kelly/Documents/work/info1903
hash -d -- WI1=/home/kelly/Documents/work/info1103
hash -d -- WE=/home/kelly/Documents/work/engg1805
hash -d -- WM1=/home/kelly/math1001
hash -d -- WM2=/home/kelly/math1002
hash -d -- WI9=/home/kelly/info1903
hash -d -- WI1=/home/kelly/info1103
hash -d -- WE=/home/kelly/engg1805

# arch pacman aliases
alias i='sudo pacman -S'
alias iu='sudo pacman -U'  

# editor
alias e="$EDITOR"
alias ed="$VISUAL"
