hy-mode
=======

Major mode for Emacs for editing buffer written in the Hy language. This mode is
only compatible with Emacs 24 and above.

Installation
============

via MELPA
---------

hy-mode is available in the MELPA repository. Do this, if MELPA isn't
already in your sources:

    (require 'package)
    (add-to-list 'package-archives
             '("MELPA" . "http://melpa.milkbox.net/packages/" ))

Then run `M-x package-refresh-contents` to load the contents of the new repository, 
and `M-x package-install RET hy-mode RET` to install `hy-mode`.


Manual
------

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

    M-x inferior-lisp

You can customize the path (also special options to the REPL like
--spy) to the Hy REPL by setting the variable
`hy-mode-inferior-lisp-command` to point to hy executable's path. This
can be done via M-x customize-group RET hy-mode and setting Hy mode
Inferior Lisp command

From your hy-mode buffer you can use the following default keybindings:

+ `C-M-x`: send the whole function (top-level form) for evaluation in the REPL
+ `C-x C-e`: send the expression at current point for evaluation
+ `C-c C-z`: switch to the lisp buffer
+ `C-c C-l`: load the file in REPL

Special Note for windows users:
-------------------------------

Depending on your PYTHONPATH and emacs path settings, invoking
inferior lisp may not work by default. You may need to customize the
path to hy repl to something like:

    (setq hy-mode-inferior-lisp-command "/path/to/python -i /path/to/hy-script.py")

About Hy
========
More information about Hy can be found at http://hylang.org
