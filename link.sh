#!/bin/bash
# Automates symlinking for each file in directory

links_vim=( vimrc "$HOME/.vim/vimrc" )
links_penta=( pentadactylrc $HOME/pentadactylrc)

# for i in ./*; do
#   if [$i == ${links_vim[0]}]; then ln -s $i ${links_vim[1]}; fi
#   if [$i == ${links_penta[0]}]; then ln -s $i ${links_penta[1]}; fi
# done

# for i in ./linux/*; do
#   ln -s $i $HOME/${i//!/\/}
# done

echo vimrc ../../.vim/vimrc
echo $i {links_vim[1]}

for i in ./*; do
  if [$i == ${links_vim[0]}]; then echo $i ${links_vim[1]}; fi
  if [$i == ${links_penta[0]}]; then echo $i ${links_penta[1]}; fi
done

for i in ./linux/*; do
  echo $i $HOME/${i//!/\/}
done
