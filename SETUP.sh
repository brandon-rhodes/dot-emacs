#!/bin/bash
# ~/.emacs.d/SETUP.sh script

# Exit immediately if a command fails.
set -ex

# Emacs 24 can no longer install Magit from melpa, but Ubuntu supplies it!
source /etc/lsb-release
if [ "$DISTRIB_RELEASE" = "16.04" ]
then
    if ! dpkg -s elpa-magit
    then
        sudo apt install -y elpa-magit
    fi
fi

# Change directory to the directory containing this script.
cd "$(dirname ${BASH_SOURCE[0]})"

# Create empty local.el if none exists.
if [ ! -f local.el ]; then touch local.el ;fi

# Unpack and install everything inside virtualenv named "~/.emacs/usr".
USR=$PWD/usr

rm -rf elpa $USR

if [ ! -d ~/.pyenv/versions/3.6.8/bin ]
then
    ~/.pyenv/bin/pyenv install 3.6.8
fi
~/.pyenv/versions/3.6.8/bin/python src/virtualenv.py $USR
source $USR/bin/activate

pip install --upgrade pip setuptools

pip install -r /dev/stdin <<'END'
black
jedi
pyflakes
python-language-server[pyflakes]
src/closure_linter-2.3.11.tar.gz
END

# python3. src/virtualenv.py $USR
# source $USR/bin/activate

# Install third-party Emacs packages.
if [ -x /Applications/Emacs.app/Contents/MacOS/Emacs ]
then
    EMACS=/Applications/Emacs.app/Contents/MacOS/Emacs
elif [ -x /usr/bin/emacs26 ]
then
    EMACS=emacs26
elif [ -x /usr/bin/emacs25 ]
then
    EMACS=emacs25
else
    EMACS=emacs
fi

$EMACS --script SETUP.el

# Byte-compile plain Emacs LISP files.
find ~/.emacs.d/site-lisp -name '*.elc' | xargs -r rm
$EMACS --batch -f batch-byte-compile ~/.emacs.d/site-lisp/*.el

echo
echo
echo SETUP.sh has completed successfully - enjoy!
echo
