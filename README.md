hy-mode
=======

Major mode for Emacs for editing buffer written in the Hy language. This mode is
only compatible with emacs24 and above.

Installation
============

To install, just copy hy-mode.el into a directory on your
load-path. Alernatively, you could always modify your load path to include a new
directory. For instance:

$ mkdir ~/.emacs.d/hy-mode
$ cp hy-mode.el ~/.emacs.d/hy-mode/hy-mode.el

In your init file, (usually `.emacs`), add this:

    ;; add the new dir to the load path
    (add-to-list 'load-path "~/.emacs.d/hy-mode")
    (require 'hy-mode)

About Hy
========
More information about Hy can be found at http://hylang.org
