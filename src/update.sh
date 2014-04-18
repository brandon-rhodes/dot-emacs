#!/bin/bash

set -e
cd "$(dirname ${BASH_SOURCE[0]})"
rm -rf virtualenv.py* virtualenv_support
t=$(tempfile) || exit
wget -O $t $(curl -s https://pypi.python.org/pypi/virtualenv |
             grep -o 'https:[^"]*tar.gz' | head -1)
tar xvz -f $t --wildcards */virtualenv.py */virtualenv_support/*
mv virtualenv-*/* .
rmdir virtualenv-*
rm $t
