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

# Byte-compile plain Emacs LISP files.
find ~/.emacs.d/site-lisp -name '*.elc' | xargs -r rm
emacs --batch -f batch-byte-compile ~/.emacs.d/site-lisp/*.el

# Create empty local.el if none exists.
if [ ! -f local.el ]; then touch local.el ;fi

# TODO: invoke straight.el for the first time to install packages.

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

echo
echo
echo SETUP.sh has completed successfully - enjoy!
echo
