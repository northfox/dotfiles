#!/bin/sh
ln -sf ~/dotfiles/.bash_profile ~/.bash_profile
ln -sf ~/dotfiles/.bashrc ~/.bashrc
ln -sf ~/dotfiles/.emacs.d ~/.emacs.d
ln -sf ~/dotfiles/.gemrc ~/.gemrc
ln -sf ~/dotfiles/.zshrc ~/.zshrc
ln -sf ~/dotfiles/.vimrc ~/.vimrc
ln -sf ~/dotfiles/.gitconfig ~/.gitconfig

if [ -d ~/.emacs.d/elisp -o ~/.emacs.d/conf -o ~/.emacs.d/public_repos ] ; then
    cd ~/.emacs.d/ && mkdir elisp conf public_repos
fi
if [ -f ~/software/eclipse/Eclipse.app/Contents/Info.plist ] ; then
    ln -sf ~/dotfiles/Info.plist ~/software/eclipse/Eclipse.app/Contents/Info.plist
fi

if [ -d ~/study/technology/repo-git/rbenv ] ; then
    ln -sf ~/study/technology/repo-git/rbenv ~/.rbenv
fi
