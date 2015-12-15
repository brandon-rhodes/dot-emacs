#!/bin/bash
#
# Update the tools here in the src directory.

cd $(dirname ${BASH_SOURCE[0]})

URL=$(
    curl -s https://pypi.python.org/pypi/virtualenv |
    pcregrep -o1 '"(https://pypi.python.org/packages/source/v/virtualenv/virtualenv-[^"]+.tar.gz)#md5=[^"]+"'
   )

rm -f virtualenv-*.tar.gz
rm -f virtualenv.py
rm -rf virtualenv_support

mkdir virtualenv_support
touch virtualenv_support/__init__.py

curl -O "$URL"
tar xvfz virtualenv-*.tar.gz
cp virtualenv-*/virtualenv.py .
cp virtualenv-*/virtualenv_support/*.whl virtualenv_support
