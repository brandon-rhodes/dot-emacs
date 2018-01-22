#!/bin/bash

# The standard Ubuntu dictionary contains too few words, so flyspell
# will have to litter draft emails and texts with "misspellings" if we
# cannot provide a better dictionary.  This command compiles one.

set -e

cd $(dirname ${BASH_SOURCE[0]})

if [ ! -x /usr/bin/aspell ]
then
    echo 'Error: /usr/bin/aspell is missing'
    echo 'Please install "aspell"'
    echo
    exit 1
fi

POOL="http://http.us.debian.org/debian/pool"
wget -c "$POOL/main/s/scowl/wamerican-huge_7.1-1_all.deb"

ar x wamerican-huge_7.1-1_all.deb data.tar.gz
tar xzf data.tar.gz --strip-components=4 \
    ./usr/share/dict/american-english-huge

sed -i '/^actually$/d' american-english-huge

rm -f ~/.emacs.d/aspell-huge

# The --local-data-dir option is necessary because of:
# https://bugs.debian.org/cgi-bin/bugreport.cgi?bug=772415

aspell \
    --local-data-dir=/usr/lib/aspell \
    --encoding=utf-8 \
    --lang=en \
    create master ~/.emacs.d/aspell-huge \
    < american-english-huge

rm american-english-huge
rm data.tar.gz
rm wamerican-huge_7.1-1_all.deb
