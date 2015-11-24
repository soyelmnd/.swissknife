#!/bin/bash

# xmonad config
if hash xmonad 2>/dev/null; then
  echo "Config xmonad ..";
  mkdir $HOME/.xmonad;
  ln -sf `pwd`/.xmonad/xmonad.hs $HOME/.xmonad/;
  ln -sf `pwd`/.xmonad/xmobar.hs $HOME/.xmonad/;
  ln -sf `pwd`/.xmonad/trayer-factory $HOME/.xmonad/;
  ln -sf `pwd`/.xmonad/feh-factory $HOME/.xmonad/;
  ln -sf `pwd`/.wallpaper $HOME/;
fi

# vim config
echo "Config vim ..";
git clone https://github.com/VundleVim/Vundle.vim.git $HOME/.vim/bundle/Vundle.vim;
ln -sf `pwd`/.vimrc $HOME/;
vim +PluginInstall +qall

# neovim config linking
if hash nvim 2>/dev/null; then
  mkdir -p ~/.config/nvim
  ln -s $HOME/.vimrc ~/.config/nvim/init.vim
  ln -s $HOME/.vim ~/.config/nvim/
fi

# terminal patched font
git clone https://github.com/powerline/fonts.git $HOME/powerline-fonts;
$HOME/powerline-fonts/install.sh;
rm -rf $HOME/powerline-fonts;

# git config
if hash git 2>/dev/null; then
  echo "Config git ..";
  ln -sf `pwd`/.gitconfig $HOME/;
fi

# bash config
echo "Config bash ..";
ln -sf `pwd`/.bash_kit $HOME/;
echo source ${HOME}/.bash_kit >> ${HOME}/.bash_profile
