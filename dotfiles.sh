#!/bin/sh
ln -sf ~/dotfiles/.bash_profile ~/.bash_profile
ln -sf ~/dotfiles/.bashrc ~/.bashrc
ln -sf ~/dotfiles/.emacs.d ~/.emacs.d
ln -sf ~/dotfiles/.gemrc ~/.gemrc
ln -sf ~/dotfiles/.zshrc ~/.zshrc
ln -sf ~/dotfiles/.vimrc ~/.vimrc

if [ -f ~/software/eclipse/Eclipse.app/Contents/Info.plist ] ; then
    ln -sf ~/dotfiles/Info.plist ~/software/eclipse/Eclipse.app/Contents/Info.plist
fi

