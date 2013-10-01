hy-mode
=======

Major mode for Emacs for editing buffer written in the Hy language. This mode is
only compatible with Emacs 24 and above.

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

Interacting with Hy from within Emacs
=====================================

When in hy-mode, you can launch a Hy REPL by launching a Lisp inferior process

    M-x lisp-inferior-process

From your hy-mode buffer you can use the following default keybindings:

+ `C-M-x`: send the whole function (top-level form) for evaluation in the REPL
+ `C-x C-e`: send the expression at current point for evaluation
+ `C-c C-z`: switch to the lisp buffer
+ `C-c C-l`: load the file in REPL

About Hy
========
More information about Hy can be found at http://hylang.org
