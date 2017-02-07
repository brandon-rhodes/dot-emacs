#!/bin/bash

# I cannot get the Chrome "Edit with Emacs" extension's "edit-server"
# library and its "(edit-server-start)" function to successfully open a
# new frame when the extension asks for one, unless the server Emacs
# already has an X Windows frame open.  So I am dropping back to the
# clunky `pycl.py` tool, which I have checked into this repository by
# which is from:
#
# https://github.com/stsquad/emacs_chrome/blob/master/servers/pycl.py
#
# With the options below, it lets me invoke the real `emacsclient`,
# which is able to launch a new X Windows window even from an Emacs
# server which has no windows open yet.

exec python2.7 ~/.emacs.d/pycl.py -e "emacsclient,--socket-name=$HOME/.emacs.d/.socket-edit-with-emacs,--alternate-editor=,--create-frame"
