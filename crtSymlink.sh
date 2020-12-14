#!/bin/sh

cd $HOME
ln -s $HOME/UbuntuDotFiles/.Xdefaults .Xdefaults
ln -s $HOME/.Xresources .Xresources
ln -s $HOME/.bash_logout .bash_logout
ln -s $HOME/.bashrc .bashrc
ln -s $HOME/.xinitrc .xinitrc
ln -s $HOME/.profile .profile
mkdir .emacs.d
ln -s $HOME/.emacs.d/init.el .emacs.d/init.el
cd $HOME/.emacs.d/
emacs --batch -f batch-byte-compile init.el
cd $HOME/.fonts/
wget -q https://github.com/miiton/Cica/releases/download/v5.0.2/Cica_v5.0.2_with_emoji.zip
unzip -q ../Cica_v5.0.2_with_emoji.zip -d Cica
