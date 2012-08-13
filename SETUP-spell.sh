#!/bin/bash

# The standard Ubuntu dictionary contains too few words, so flyspell
# will have to litter draft emails and texts with "misspellings" if we
# cannot provide a better dictionary.  This command compiles one.

set -e

if [ ! -x /usr/bin/aspell ]
then
    echo 'Error: /usr/bin/aspell is missing'
    echo 'Please install "aspell"'
    echo
    exit 1
fi

if [ ! -f /usr/share/dict/american-english-huge ]
then
    echo 'Error: /usr/share/dict/american-english-huge missing'
    echo 'Please install wamerican-huge'
    echo
    exit 1
fi

aspell --encoding=utf-8 --lang=en create master ~/.emacs.d/aspell-huge \
    < /usr/share/dict/american-english-huge
