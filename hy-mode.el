;;; hy-mode.el --- Major mode for Hylang

;; Copyright © 2013 Julien Danjou <julien@danjou.info>
;;           © 2017 Eric Kaschalk <ekaschalk@gmail.com>
;;
;; Authors: Julien Danjou <julien@danjou.info>
;;          Eric Kaschalk <ekaschalk@gmail.com>
;; URL: http://github.com/hylang/hy-mode
;; Version: 1.0
;; Keywords: languages, lisp, python
;; Package-Requires: ((dash "2.13.0") (dash-functional "1.2.0") (s "1.11.0") (emacs "24"))

;; hy-mode is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; hy-mode is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with hy-mode.  If not, see <http://www.gnu.org/licenses/>.

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Provides font-lock, indentation, and navigation for the Hy
;; language. (http://hylang.org)

(require 'dash)
(require 'dash-functional)
(require 's)

(defgroup hy-mode nil
  "A mode for Hy"
  :prefix "hy-mode-"
  :group 'applications)

(defcustom hy-mode-inferior-lisp-command "hy"
  "The command used by `inferior-lisp-program'."
  :type 'string
  :group 'hy-mode)

;;; Keywords

(defconst hy--kwds-anaphorics
  '("ap-if" "ap-each" "ap-each-while" "ap-map" "ap-map-when" "ap-filter"
    "ap-reject" "ap-dotimes" "ap-first" "ap-last" "ap-reduce" "ap-pipe"
    "ap-compose" "xi")

  "Hy anaphoric contrib keywords.")

(defconst hy--kwds-builtins
  '("*map" "accumulate" "and" "assoc" "butlast" "calling-module-name" "car"
    "cdr" "chain" "coll?" "combinations" "comp" "complement" "compress" "cons"
    "cons?" "constantly" "count" "cut" "cycle" "dec" "def" "defmain" "del"
    "dict-comp" "disassemble" "distinct" "doto" "drop" "drop-last" "drop-while"
    "empty?" "even?" "every?" "filter" "first" "flatten" "float?" "fraction"
    "genexpr" "gensym" "get" "group-by" "identity" "inc" "input"
    "instance?" "integer" "integer-char?" "integer?" "interleave" "interpose"
    "is" "is-not" "is_not" "islice" "iterable?" "iterate" "iterator?" "juxt"
    "keyword" "keyword?" "last" "list*" "list-comp" "macroexpand"
    "macroexpand-1" "map" "merge-with" "multicombinations" "name" "neg?" "none?"
    "nth" "numeric?" "odd?" "or" "partition" "permutations"
    "pos?" "product" "quasiquote" "quote" "range" "read" "read-str"
    "reduce" "remove" "repeat" "repeatedly" "rest" "second" "setv" "set-comp"
    "slice" "some" "string" "string?" "symbol?" "take" "take-nth" "take-while"
    "tee" "unquote" "unquote-splice" "xor" "zero?" "zip" "zip-longest"

    ;; Pure python builtins
    "abs" "all" "any" "bin" "bool" "callable" "chr"
    "compile" "complex" "delattr" "dict" "dir" "divmod" "enumerate"
    "eval" "float" "format" "frozenset" "getattr" "globals" "hasattr"
    "hash" "help" "hex" "id" "isinstance" "issubclass" "iter" "len"
    "list" "locals" "max" "memoryview" "min" "next" "object" "oct" "open"
    "ord" "pow" "repr" "reversed" "round" "set" "setattr"
    "sorted" "str" "sum" "super" "tuple" "type" "vars"
    "ascii" "bytearray" "bytes" "exec"
    "--package--" "__package__" "--import--" "__import__"
    "--all--" "__all__" "--doc--" "__doc__" "--name--" "__name__")

  "Hy builtin keywords.")

(defconst hy--kwds-constants
  '("True" "False" "None"
    "Ellipsis"
    "NotImplemented"
    "nil"  ; For those that alias None as nil
    )

  "Hy constant keywords.")

(defconst hy--kwds-exceptions
  '("ArithmeticError" "AssertionError" "AttributeError" "BaseException"
    "DeprecationWarning" "EOFError" "EnvironmentError" "Exception"
    "FloatingPointError" "FutureWarning" "GeneratorExit" "IOError"
    "ImportError" "ImportWarning" "IndexError" "KeyError"
    "KeyboardInterrupt" "LookupError" "MemoryError" "NameError"
    "NotImplementedError" "OSError" "OverflowError"
    "PendingDeprecationWarning" "ReferenceError" "RuntimeError"
    "RuntimeWarning" "StopIteration" "SyntaxError" "SyntaxWarning"
    "SystemError" "SystemExit" "TypeError" "UnboundLocalError"
    "UnicodeDecodeError" "UnicodeEncodeError" "UnicodeError"
    "UnicodeTranslateError" "UnicodeWarning" "UserWarning" "VMSError"
    "ValueError" "Warning" "WindowsError" "ZeroDivisionError"
    "BufferError" "BytesWarning" "IndentationError" "ResourceWarning" "TabError")

  "Hy exception keywords.")

(defconst hy--kwds-defs
  '("defn" "defun"
    "defmacro" "defmacro/g!" "defmacro!"
    "defreader" "defsharp" "deftag")

  "Hy definition keywords.")

(defconst hy--kwds-operators
  '("!=" "%" "%=" "&" "&=" "*" "**" "**=" "*=" "+" "+=" "," "-"
    "-=" "/" "//" "//=" "/=" "<" "<<" "<<=" "<=" "=" ">" ">=" ">>" ">>="
    "^" "^=" "|" "|=" "~")

  "Hy operator keywords.")

(defconst hy--kwds-special-forms
  '(;; Looping
    "loop" "recur"
    "for" "for*"

    ;; Threading
    "->" "->>" "as->"

    ;; Flow control
    "return"
    "if" "if*" "if-not" "lif" "lif-not"
    "else" "unless" "when"
    "break" "continue"
    "while" "cond"
    "do" "progn"

    ;; Functional
    "lambda" "fn"
    "yield" "yield-from"
    "with" "with*"
    "with-gensyms"

    ;; Error Handling
    "except" "try" "throw" "raise" "catch" "finally" "assert"

    ;; Special
    "print"
    "not"
    "in" "not-in"

    ;; Misc
    "global" "nonlocal"
    "eval" "eval-and-compile" "eval-when-compile"

    ;; Discontinued in Master
    "apply" "kwapply")

  "Hy special forms keywords.")

;;; Font Locks
;;;; Definitions

(defconst hy--font-lock-kwds-builtins
  (list
   (rx-to-string
    `(: (not (any "#"))
        symbol-start
        (or ,@hy--kwds-operators
            ,@hy--kwds-builtins
            ,@hy--kwds-anaphorics)
        symbol-end))

   '(0 font-lock-builtin-face))

  "Hy builtin keywords.")

(defconst hy--font-lock-kwds-constants
  (list
   (rx-to-string
    `(: (or ,@hy--kwds-constants)))

   '(0 font-lock-constant-face))

  "Hy constant keywords.")

(defconst hy--font-lock-kwds-defs
  (list
   (rx-to-string
    `(: (group-n 1 (or ,@hy--kwds-defs))
        (1+ space)
        (group-n 2 (1+ word))))

   '(1 font-lock-keyword-face)
   '(2 font-lock-function-name-face nil t))

  "Hy definition keywords.")

(defconst hy--font-lock-kwds-exceptions
  (list
   (rx-to-string
    `(: symbol-start
        (or ,@hy--kwds-exceptions)
        symbol-end))

   '(0 font-lock-type-face))

  "Hy exception keywords.")

(defconst hy--font-lock-kwds-special-forms
  (list
   (rx-to-string
    `(: symbol-start
        (or ,@hy--kwds-special-forms)
        symbol-end))

   '(0 font-lock-keyword-face))

  "Hy special forms keywords.")

;;;; Static

(defconst hy--font-lock-kwds-aliases
  (list
   (rx (group-n 1 (or "defmacro-alias" "defn-alias" "defun-alias"))
       (1+ space)
       "["
       (group-n 2 (1+ anything))
       "]")

   '(1 font-lock-keyword-face)
   '(2 font-lock-function-name-face nil t))

  "Hy aliasing keywords.")

(defconst hy--font-lock-kwds-class
  (list
   (rx (group-n 1 "defclass")
       (1+ space)
       (group-n 2 (1+ word)))

   '(1 font-lock-keyword-face)
   '(2 font-lock-type-face))

  "Hy class keywords.")

(defconst hy--font-lock-kwds-decorators
  (list
   (rx
    (or (: "#@"
           (syntax open-parenthesis))
        (: symbol-start
           "with-decorator"
           symbol-end
           (1+ space)))
    (1+ word))

   '(0 font-lock-type-face))

  "Hylight the symbol after `#@' or `with-decorator' macros.")

(defconst hy--font-lock-kwds-imports
  (list
   (rx (or "import" "require" ":as")
       (or (1+ space) eol))

   '(0 font-lock-keyword-face))

  "Hy import keywords.")

(defconst hy--font-lock-kwds-self
  (list
   (rx symbol-start
       (group "self")
       (or "." symbol-end))

   '(1 font-lock-keyword-face))

  "Hy self keyword.")

(defconst hy--font-lock-kwds-tag-macros
  (list
   (rx "#"
       (not (any "*" "@"))
       (0+ word))

   '(0 font-lock-function-name-face))

  "Hylight tag macros, ie. `#tag-macro', so they stand out.")

(defconst hy--font-lock-kwds-variables
  (list
   (rx symbol-start
       (or "setv" "def")
       symbol-end
       (1+ space)
       (group (1+ word)))

   '(1 font-lock-variable-name-face))

  "Hylight variable names in setv/def, only first name.")

;;;; Misc

(defconst hy--font-lock-kwds-func-modifiers
  (list
   (rx symbol-start "&" (1+ word))

   '(0 font-lock-type-face))

  "Hy '&rest/&kwonly/...' styling.")

(defconst hy--font-lock-kwds-kwargs
  (list
   (rx symbol-start ":" (1+ word))

   '(0 font-lock-constant-face))

  "Hy ':kwarg' styling.")

(defconst hy--font-lock-kwds-shebang
  (list
   (rx buffer-start "#!" (0+ not-newline) eol)

   '(0 font-lock-comment-face))

  "Hy shebang line.")

(defconst hy--font-lock-kwds-unpacking
  (list
   (rx (or "#*" "#**")
       symbol-end)

   '(0 font-lock-keyword-face))

  "Hy #* arg and #** kwarg unpacking keywords.")

;;;; Grouped

(defconst hy-font-lock-kwds
  (list hy--font-lock-kwds-aliases
        hy--font-lock-kwds-builtins
        hy--font-lock-kwds-class
        hy--font-lock-kwds-constants
        hy--font-lock-kwds-defs
        hy--font-lock-kwds-decorators
        hy--font-lock-kwds-exceptions
        hy--font-lock-kwds-func-modifiers
        hy--font-lock-kwds-imports
        hy--font-lock-kwds-kwargs
        hy--font-lock-kwds-self
        hy--font-lock-kwds-shebang
        hy--font-lock-kwds-special-forms
        hy--font-lock-kwds-tag-macros
        hy--font-lock-kwds-unpacking
        hy--font-lock-kwds-variables)

  "All Hy font lock keywords.")

;;; Indentation
;;;; Specform

(defconst hy-indent-special-forms
  '(:exact
    ("if" "if-not"
     "when" "unless"
     "for" "for*"
     "while"
     "except" "catch")

    :fuzzy
    ("def"
     "with"
     "fn"
     "lambda"))
  "Special forms to indent 1.")

;;;; Utilities

;; Aliases for `parse-partial-sexp' value
(defun hy--sexp-inermost-char (state)
  (nth 1 state))
(defun hy--start-of-last-sexp (state)
  (nth 2 state))
(defun hy--prior-sexp? (state)
  (number-or-marker-p (hy--start-of-last-sexp state)))

(defun hy--anything-before? (pos)
  "Determine if chars before POS in current line."
  (s-matches? (rx (not blank))
              (buffer-substring (line-beginning-position) pos)))

(defun hy--anything-after? (pos)
  "Determine if POS is before line-end-position."
  (when pos
    (< pos (line-end-position))))

(defun hy--check-non-symbol-sexp (pos)
  "Check for a non-symbol yet symbol-like (tuple constructor comma) at POS."
  (member (char-after pos) '(?\,)))

;;;; Normal Indent

(defun hy--normal-indent (last-sexp)
  "Determine normal indentation column of LAST-SEXP.

Example:
 (a (b c d
       e
       f))

1. Indent e => start at d -> c -> b.
Then backwards-sexp will throw error trying to jump to a.
Observe 'a' need not be on the same line as the ( will cause a match.
Then we determine indentation based on whether there is an arg or not.

2. Indenting f will go to e.
Now since there is a prior sexp d but we have no sexps-before on same line,
the loop will terminate without error and the prior lines indentation is it."
  (goto-char last-sexp)
  (-let [last-sexp-start nil]
    (if (ignore-errors
          (while (hy--anything-before? (point))
            (setq last-sexp-start (prog1
                                      ;; Indentation should ignore quote chars
                                      (if (-contains? '(?\' ?\` ?\~)
                                                      (char-before))
                                          (1- (point))
                                        (point))
                                    (backward-sexp))))
          t)
        (current-column)
      (if (not (hy--anything-after? last-sexp-start))
          (1+ (current-column))
        (goto-char last-sexp-start)  ; Align with function argument
        (current-column)))))

;;;; Function or form

(defun hy--not-function-form-p ()
  "Non-nil if form at point doesn't represent a function call."
  (unless (hy--check-non-symbol-sexp (1+ (point)))  ; tuple constructor special
    (or (-contains? '(?\[ ?\{) (char-after))
        (not (looking-at (rx anything  ; Skips form opener
                             (or (syntax symbol) (syntax word))))))))

;;;; Hy find indent spec

(defun hy--find-indent-spec (state)
  "Return integer for special indentation of form or nil to use normal indent.

Note that `hy--not-function-form-p' filters out forms that are lists and dicts.
Point is always at the start of a function."
  (-when-let
      (function (and (hy--prior-sexp? state)
                     (thing-at-point 'symbol)))

    (or (-contains? (plist-get hy-indent-special-forms :exact)
                    function)
        (-some (-cut s-matches? <> function)
               (plist-get hy-indent-special-forms :fuzzy)))))

;;;; Hy indent function

(defun hy-indent-function (indent-point state)
  "Indent at INDENT-POINT where STATE is `parse-partial-sexp' for INDENT-POINT."
  (goto-char (hy--sexp-inermost-char state))

  (if (hy--not-function-form-p)
      (1+ (current-column))  ; Indent after [, {, ... is always 1
    (forward-char 1)  ; Move to start of sexp

    (cond ((hy--check-non-symbol-sexp (point))  ; Comma tuple constructor
           (+ 2 (current-column)))

          ((hy--find-indent-spec state)  ; Special form uses fixed indendation
           (1+ (current-column)))

          (t
           (hy--normal-indent calculate-lisp-indent-last-sexp)))))

;;; Syntax

(defconst hy-mode-syntax-table
  (-let [table (copy-syntax-table lisp-mode-syntax-table)]
    (modify-syntax-entry ?\{ "(}" table)
    (modify-syntax-entry ?\} "){" table)
    (modify-syntax-entry ?\[ "(]" table)
    (modify-syntax-entry ?\] ")[" table)
    (modify-syntax-entry ?\~ "'" table)

    table)
  "Hy modes syntax table.")

;;; Font Lock Docs

(defun hy-string-in-doc-position-p (listbeg startpos)
  "Return true if a doc string may occur at STARTPOS inside a list.
LISTBEG is the position of the start of the innermost list
containing STARTPOS."
  (if (= 1 startpos)  ; Uniquely identifies module docstring
      t
    (let* ((firstsym (and listbeg
                          (save-excursion
                            (goto-char listbeg)
                            (and (looking-at
                                  (eval-when-compile
                                    (concat "([ \t\n]*\\("
                                            lisp-mode-symbol-regexp "\\)")))
                                 (match-string-no-properties 1))))))

      (or (member firstsym hy--kwds-defs)
          (string= firstsym "defclass")))))

(defun hy-font-lock-syntactic-face-function (state)
  "Return syntactic face function for the position represented by STATE.
STATE is a `parse-partial-sexp' state, and the returned function is the
Lisp font lock syntactic face function."
  (if (nth 3 state)
      ;; This might be a (doc)string or a |...| symbol.
      (let ((startpos (nth 8 state)))
        (if (eq (char-after startpos) ?|)
            ;; This is not a string, but a |...| symbol.
            nil
          (let ((listbeg (nth 1 state)))
            (if (hy-string-in-doc-position-p listbeg startpos)
                font-lock-doc-face
              font-lock-string-face))))
    font-lock-comment-face))

;;; Hy-mode

;;;###autoload
(progn
  (add-to-list 'auto-mode-alist '("\\.hy\\'" . hy-mode))
  (add-to-list 'interpreter-mode-alist '("hy" . hy-mode)))

;;;###autoload
(define-derived-mode hy-mode prog-mode "Hy"
  "Major mode for editing Hy files."
  (setq font-lock-defaults
        '(hy-font-lock-kwds
          nil nil
          (("+-*/.<>=!?$%_&~^:@" . "w")) ; syntax alist
          nil
          (font-lock-mark-block-function . mark-defun)
          (font-lock-syntactic-face-function
           . hy-font-lock-syntactic-face-function)))

  (setq-local ahs-include "^[0-9A-Za-z/_.,:;*+=&%|$#@!^?-~]+$")

  ;; Smartparens
  (when (fboundp 'sp-local-pair)
    (sp-local-pair '(hy-mode) "`" "`" :actions nil)
    (sp-local-pair '(hy-mode) "'" "'" :actions nil))

  ;; Fixes #43: inferior lisp history getting corrupted
  ;; Ideally change so original comint-stored-incomplete-input functionality
  ;; is preserved for terminal case, but not big deal.
  ;; TODO This should only operate on hy mode comints!
  (advice-add 'comint-previous-input :before
              (lambda (&rest args) (setq-local comint-stored-incomplete-input "")))

  ;; Comments
  (setq-local comment-start ";")
  (setq-local comment-start-skip
              "\\(\\(^\\|[^\\\\\n]\\)\\(\\\\\\\\\\)*\\)\\(;+\\|#|\\) *")
  (setq-local comment-add 1)

  ;; Indentation
  (setq-local indent-tabs-mode nil)
  (setq-local indent-line-function 'lisp-indent-line)
  (setq-local lisp-indent-function 'hy-indent-function)

  ;; Inferior Lisp Program
  (setq-local inferior-lisp-program hy-mode-inferior-lisp-command)
  (setq-local inferior-lisp-load-command
              (concat "(import [hy.importer [import-file-to-module]])\n"
                      "(import-file-to-module \"__main__\" \"%s\")\n"))

  (setenv "PYTHONIOENCODING" "UTF-8"))

;;; Utilities

;;;###autoload
(defun hy-insert-pdb ()
  "Import and set pdb trace at point."
  (interactive)
  (insert "(do (import pdb) (pdb.set-trace))"))

;;;###autoload
(defun hy-insert-pdb-threaded ()
  "Import and set pdb trace at point for a threading macro."
  (interactive)
  (insert "((fn [x] (import pdb) (pdb.set-trace) x))"))

;;; Keybindings

(set-keymap-parent hy-mode-map lisp-mode-shared-map)
(define-key hy-mode-map (kbd "C-M-x")   'lisp-eval-defun)
(define-key hy-mode-map (kbd "C-x C-e") 'lisp-eval-last-sexp)
(define-key hy-mode-map (kbd "C-c C-z") 'switch-to-lisp)
(define-key hy-mode-map (kbd "C-c C-l") 'lisp-load-file)

(define-key hy-mode-map (kbd "C-c C-t") 'hy-insert-pdb)
(define-key hy-mode-map (kbd "C-c C-S-t") 'hy-insert-pdb-threaded)

(provide 'hy-mode)

;;; hy-mode.el ends here
