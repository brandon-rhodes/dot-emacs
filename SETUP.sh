#!/bin/bash
# ~/.emacs.d/SETUP.sh script

# Exit immediately if a command fails.
set -e

# Change directory to the directory containing this script.
cd "$(dirname ${BASH_SOURCE[0]})"

# Create empty local.el if none exists.
if [ ! -f local.el ]; then touch local.el ;fi

# Unpack and install everything inside virtualenv named "~/.emacs/usr".
USR=$PWD/usr

rm -rf $USR

python2.7 src/virtualenv.py $USR
source $USR/bin/activate

pip install --upgrade pip

pip install src/closure_linter-2.3.11.tar.gz
pip install pyflakes
pip install jedi
pip install epc

# Install third-party Emacs packages.
if [ -x /Applications/Emacs.app/Contents/MacOS/Emacs ]
then
    EMACS=/Applications/Emacs.app/Contents/MacOS/Emacs
elif [ -x /usr/bin/emacs25 ]
then
    EMACS=emacs25
else
    EMACS=emacs24
fi

$EMACS --script SETUP.el

# Byte-compile plain Emacs LISP files.
$EMACS --batch -f batch-byte-compile ~/.emacs.d/site-lisp/*.el

echo
echo
echo SETUP.sh has completed successfully - enjoy!
echo
