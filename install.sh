#!/bin/bash

# xmonad config
echo "Config xmonad ..";
mkdir $HOME/.xmonad;
ln -sf `pwd`/.xmonad/xmonad.hs $HOME/.xmonad/;
ln -sf `pwd`/.xmonad/xmobar.hs $HOME/.xmonad/;
ln -sf `pwd`/.xmonad/trayer-factory $HOME/.xmonad/;
ln -sf `pwd`/.xmonad/feh-factory $HOME/.xmonad/;
ln -sf `pwd`/.wallpaper $HOME/;

# vim config
echo "Config vim ..";
git clone https://github.com/VundleVim/Vundle.vim.git $HOME/.vim/bundle/Vundle.vim;
ln -sf `pwd`/.vimrc $HOME/;
vim +PluginInstall +qall

# terminal patched font
git clone https://github.com/powerline/fonts.git $HOME/powerline-fonts;
$HOME/powerline-fonts/install.sh;
rm -rf $HOME/powerline-fonts;

# git config
echo "Config git ..";
ln -sf `pwd`/.gitconfig $HOME/;

# bash config
echo "Config bash ..";
ln -sf `pwd`/.bash_aliases $HOME/;
echo . ${HOME}/.bash_aliases >> ${HOME}/.bashrc
