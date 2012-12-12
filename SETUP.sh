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

DIRS=(Pymacs-0.23)
rm -rf $USR $DIRS

python2 src/virtualenv.py $USR
source $USR/bin/activate

pip install src/closure_linter-2.3.5.tar.gz
pip install src/pyflakes-0.5.0.tar.gz
pip install src/rope-0.9.3.tar.gz
pip install src/ropemacs-0.6.tar.gz

# Copy pymacs.el to ~/.emacs.d
tar xvfz src/Pymacs.tar.gz
(cd Pymacs-0.23; python setup.py install)
rm -f ./site-lisp/pymacs.el
cp Pymacs-0.23/pymacs.el ./site-lisp/pymacs.el
rm -rf $PWD/Pymacs-0.23

# Byte-compile plain Emacs LISP files.
emacs --batch -f batch-byte-compile ~/.emacs.d/site-lisp/*.el

echo
echo
echo SETUP.sh has completed successfully - enjoy!
echo
