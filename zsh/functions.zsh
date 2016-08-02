# download a package from AUR and attempt to build it
# make required directories before moving
function md() {
  dest="${@:$#}"
  dir="${$dest%/*}"
  echo "$dir" "${@:1:$# -1}" "$dest"
  #/bin/mkdir -vp "$dir" && /bin/mv -i "${@:1:$# -1}" "$dest"
}

# merge multiple pdfs into one
# gsmerge output.pdf input1.pdf input2.pdf
function gsmerge() { gs -dBATCH -dNOPAUSE -q -sDEVICE=pdfwrite -sOutputFile=$1 "${@:2}" }
