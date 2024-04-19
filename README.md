dot-emacs
=========

This repository contains my `.emacs.d` directory, whose configuration is
focused on making Python and JavaScript development more fun.  You might
also be interested in looking at my version controlled home directory,
which is kept here:

https://github.com/brandon-rhodes/homedir

To install this `.emacs.d` directory under your own an account, use the
following commands to check out this directory, move it into place, and
run its setup script:

    $ cd ~
    $ if [ -d .emacs.d ] ;then mv .emacs.d old-emacs.d ;fi
    $ git clone https://github.com/brandon-rhodes/dot-emacs.git .emacs.d
    $ .emacs.d/SETUP.sh

You will then be ready to run Emacs!

The `SETUP.sh` creates a Python virtual environment down inside of the
directory and install several programs that give Emacs some IDE super
powers when editing Python code.  To get a good survey of the way my
environment is customzied, first read the comments in `init.el`, and
then look up specific settings if you have questions.  You can first try
looking up each customization variable inside of Emacs by running `M-x`
`customize-apropos` followed by part of its name.  If the brief Emacs
docstring does not give you enough information, then the next best place
is usually a Google search for "Emacs wiki <variable-name>" which will
hopefully unlock a trove of information at http://emacswiki.org.
