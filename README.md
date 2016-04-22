hy-mode
=======

Major mode for Emacs for editing buffer written in the [Hy language](http://docs.hylang.org/en/latest/). This mode
is only compatible with Emacs 24 and above.


Installation
============

via MELPA
---------

`hy-mode` is available in the MELPA repository. Do this, if MELPA isn't
already in your sources:

```el
(require 'package)
(add-to-list 'package-archives
             '("MELPA" . "https://melpa.org/packages/" ))
```

Then run `M-x package-refresh-contents` to load the contents of the new
repository, and `M-x package-install RET hy-mode RET` to install `hy-mode`.


Manual
------

To install, just copy `hy-mode.el` into a directory on your
load path. Alernatively, you could always modify your load path to include a
new directory. For instance:

```sh
$ mkdir ~/.emacs.d/hy-mode
$ cp hy-mode.el ~/.emacs.d/hy-mode/hy-mode.el
```

In your init file, (usually `.emacs` or `.emacs.d/init.el`), add this:

```el
;; add the new dir to the load path
(add-to-list 'load-path "~/.emacs.d/hy-mode")
(require 'hy-mode)
```

Interacting with Hy from within Emacs
=====================================

When in `hy-mode`, you can launch a Hy REPL by launching a Lisp inferior
process:

```
M-x inferior-lisp
```

You can customize the path (also special options to the REPL like
`--spy`) to the Hy REPL by setting the variable
`hy-mode-inferior-lisp-command` to point to Hy executable's path. This
can be done via `M-x customize-group RET hy-mode` and setting `hy-mode`
inferior Lisp command.

From your `hy-mode` buffer you can use the following default keybindings:

+ `C-M-x`: send the whole function (top-level form) for evaluation in the REPL
+ `C-x C-e`: send the expression at current point for evaluation
+ `C-c C-z`: switch to the lisp buffer
+ `C-c C-l`: load the file in REPL

Other recommended settings
==========================

Since `hy-mode` is a lisp like mode, it can use the goodness from some
minor modes used for lisp such as
[smartparens](https://github.com/Fuco1/smartparens) or paredit minor
modes which support excellent movement and navigation ability other
than keeping the s-expressions balanced.

```el
; eg: Adding paredit minor mode
(add-hook 'hy-mode-hook 'paredit-mode)

; Add smartparens-strict-mode
(add-hook 'hy-mode-hook #'smartparens-strict-mode)

; alternatively you could enable this to a bunch of lisp modes
(dolist (lisp-mode '(scheme emacs-lisp lisp clojure hy))
  (add-hook (intern (concat (symbol-name lisp-mode) "-mode-hook"))
		'paredit-mode))

```

[RainbowDelimiters](https://github.com/Fanael/rainbow-delimiters) is another minor mode which highlights parentheses, brackets, and braces according to their depth. Each successive level is highlighted in a different color. This makes it easy to spot matching delimiters, orient yourself in the code, and tell which statements are at a given depth. Assuming you've already installed RainbowDelimiters you can enable it like this:

```el
(add-hook 'hy-mode-hook #'rainbow-delimiters-mode)

```

Special note for Windows users:
-------------------------------

Depending on your `PYTHONPATH` and Emacs path settings, invoking
inferior Lisp may not work by default. You may need to customize the
path to Hy REPL to something like:

```el
(setq hy-mode-inferior-lisp-command "/path/to/python -i /path/to/hy-script.py")
```

About Hy
========

More information about Hy can be found at http://hylang.org.
