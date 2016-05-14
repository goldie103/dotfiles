# download a package from AUR and attempt to build it
# aur discord
aur () {
  cd $HOME/src
  git clone https://aur.archlinux.org/"$1".git
  cd "$1"
  makepkg -s
}

# make required directories before moving
md () { mkdir -vp "$(dirname $2)" && mv -i "$@" }

# merge multiple pdfs into one
# gsmerge output.pdf input1.pdf input2.pdf
gsmerge () { gs -dBATCH -dNOPAUSE -q -sDEVICE=pdfwrite -sOutputFile=$1 "${@:2}" }
