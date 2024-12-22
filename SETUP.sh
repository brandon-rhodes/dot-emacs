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

# Verify Emacs version.
if ! emacs --version | grep 'GNU Emacs 29'
then
    echo -e '\nWrong Emacs version; please install Emacs 29\n'
    exit 1
fi

# Change directory to the directory containing this script.
cd "$(dirname ${BASH_SOURCE[0]})"

mkdir -p third-party

# To rebuild the 'sums' file that drives this loop, run:
#
# (cd third-party && sha256sum *.tar) > sums

while read -r signature tarfile
do
    if [ ! -f third-party/$tarfile ]
    then
        cd third-party
        if [[ "$tarfile" == compat* ]]
        then
            url=https://elpa.gnu.org/packages
        else
            url=https://melpa.org/packages
        fi
        wget $url/$tarfile
        cd ..
    fi
done < sums

cd third-party
sha256sum -c ../sums
cd ..

for tarfile in $(cd third-party && echo *.tar)
do
    package=${tarfile%.tar}
    if [ ! -d elpa/$package ]
    then
        emacs --batch --eval '(package-install-file "third-party/'$tarfile'")'
    fi
done

if [ ! -f venv/bin/activate ]
then
    uv venv venv
fi
source venv/bin/activate

# This takes Emacs startup time from 1.1s to 1.5s on a Python file.

uv pip install python-lsp-server==1.11.0 $(cat <<EOF
  docstring-to-markdown==0.15
  importlib-metadata==7.1.0
  jedi==0.19.1
  parso==0.8.4
  pluggy==1.4.0
  python-lsp-jsonrpc==1.1.2
  ujson==5.9.0
  zipp==3.18.1
EOF
)

# This adds <0.1s.

uv pip install python-lsp-isort==0.1 isort==5.13.2

# This takes startup time from 1.6s to >2.2s.  Gads.  Can that be improved?

uv pip install ruff==0.4.1
uv pip install python-lsp-ruff==2.2.0 $(cat <<EOF
  attrs==23.2.0
  cattrs==23.2.3
  exceptiongroup==1.2.1
  lsprotocol==2023.0.1
  tomli==2.0.1
  typing-extensions==4.11.0
EOF
)

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
