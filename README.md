dot-emacs
=========

My `.emacs.d` directory, with attention paid to Python and JavaScript
development.

To install it under an account, first check out this directory as
`~/.emacs.d/` and then run its setup shell script:

    $ cd ~
    $ git clone https://github.com/brandon-rhodes/dot-emacs.git
    $ mv dot-emacs .emacs.d
    $ .emacs.d/SETUP.sh

You can then run Emacs.

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
