#!/bin/bash
cp -f ~/.emacs ./
cp -f ~/.emacs.d/config/* ./.emacs.d/config/
rm ./*~
rm ./.emacs.d/config/*~
