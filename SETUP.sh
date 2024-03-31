#!/bin/bash
# ~/.emacs.d/SETUP.sh script

make_it_more_obvious_we_failed() {
    rv=$?
    if [ "$rv" != "0" ]
    then
        echo
        echo ERROR: Setup failed
        echo
    fi
    exit $rv
}
trap '{ set +x; } 2>/dev/null && make_it_more_obvious_we_failed' 0

# Exit immediately if a command fails.
set -ex

mkdir -p third-party

for i in \
    "https://github.com/minad/consult 1.4" \
    "https://github.com/bling/fzf.el master"
do
    set -- $i
    url=$1
    version=$2
    d=$(basename $url)
    if [ ! -d third-party/$d ]
    then
        git clone \
            --depth 1 \
            --branch $version \
            $url \
            third-party/$d
    fi
done

if [ ! -d third-party/dtrt-indent ]
then
    url=https://github.com/jscheid/dtrt-indent
    git clone \
        --depth 1 \
        --branch 1.17 \
        $url \
        third-party/dtrt-indent
    # e45fa76
fi

if [ ! -d third-party/vertico ]
then
    url=https://github.com/minad/vertico
    git clone \
        --depth 1 \
        --branch 1.7 \
        $url \
        third-party/vertico
fi

if [ ! -d third-party/compat ]
then
    url=https://github.com/emacs-compat/compat
    git clone \
        --depth 1 \
        --branch 29.1.4.5 \
        $url \
        third-party/compat
fi

#https://github.com/emacs-compat/compat

exit



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

~/.pyenv/bin/pyenv install --skip-existing 3.6.8
~/.pyenv/versions/3.6.8/bin/python src/virtualenv.py $USR
source $USR/bin/activate

pip install --upgrade 'pip<22' setuptools

pip install -r /dev/stdin <<'END'
black
epc
jedi
pyflakes
END

# src/closure_linter-2.3.11.tar.gz

# Install third-party Emacs packages.
if [ -x /Applications/Emacs.app/Contents/MacOS/Emacs ]
then
    EMACS=/Applications/Emacs.app/Contents/MacOS/Emacs
elif [ -x /usr/bin/emacs26 ]
then
    EMACS=emacs26
elif [ -x /usr/bin/emacs-gtk ]
then
    EMACS=emacs-gtk
elif [ -x /usr/bin/emacs25 ]
then
    EMACS=emacs25
else
    EMACS=emacs
fi

$EMACS --script setup/phase1.el

# Force update of elpa key.
FINGERPRINT=066DAFCB81E42C40
if ! gpg --homedir ~/.emacs.d/elpa/gnupg --list-keys $FINGERPRINT
then
    gpg --homedir ~/.emacs.d/elpa/gnupg --receive-keys $FINGERPRINT
fi

$EMACS --script setup/phase2.el

# Byte-compile plain Emacs LISP files.
find ~/.emacs.d/site-lisp -name '*.elc' | xargs -r rm
$EMACS --batch -f batch-byte-compile ~/.emacs.d/site-lisp/*.el

echo
echo
echo SETUP.sh has completed successfully - enjoy!
echo
