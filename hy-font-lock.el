;;; hy-font-lock.el --- Font Locks -*- lexical-binding: t -*-

;; Copyright © 2013 Julien Danjou <julien@danjou.info>
;;           © 2017-2019 Eric Kaschalk <ekaschalk@gmail.com>
;;
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

;; Font lock definitions and setup for `hy-mode'.

;; Font locks are organized and exposed at the end: `hy-font-lock-kwds'
;; Comint Font locks are exposed in: `inferior-hy-font-lock-kwds'
;; Also implements docstring detection: `hy-font-lock-syntactic-face-function'

;;; Code:

(require 'hy-base)

(defvar hy-font-lock-highlight-percent-args? t
  "Whether to highlight '%i' symbols in Hy's clojure-like syntax for lambdas.")

;;; Names Lists
;;;; Hy Builtins

(defconst hy-font-lock--hy-builtins
  '("*map"
    "accumulate"
    "assoc"
    "butlast"
    "calling-module-name"
    "chain"
    "coll?"
    "combinations"
    "comp"
    "complement"
    "compress"
    "constantly"
    "count"
    "cut"
    "cycle"
    "dec"
    "defmain"
    "del"
    "disassemble"
    "distinct"
    "doto"
    "drop"
    "drop-last"
    "drop-while"
    "empty?"
    "even?"
    "every?"
    "filter"
    "first"
    "flatten"
    "float?"
    "fraction"
    "gensym"
    "get"
    "group-by"
    "identity"
    "inc"
    "instance?"
    "integer"
    "integer-char?"
    "integer?"
    "interleave"
    "interpose"
    "is"
    "is-not"
    "islice"
    "iterable?"
    "iterate"
    "iterator?"
    "juxt"
    "keyword"
    "keyword?"
    "last"
    "macroexpand"
    "macroexpand-1"
    "merge-with"
    "multicombinations"
    "name"
    "neg?"
    "none?"
    "nth"
    "numeric?"
    "odd?"
    "partition"
    "permutations"
    "pos?"
    "product"
    "quasiquote"
    "quote"
    "read"
    "read-str"
    "reduce"
    "remove"
    "repeat"
    "repeatedly"
    "rest"
    "second"
    "setv"
    "some"
    "string"
    "string?"
    "symbol?"
    "take"
    "take-nth"
    "take-while"
    "tee"
    "unquote"
    "unquote-splice"
    "xor"
    "zero?"
    "zip"
    "zip-longest"
    "--macros--" "__macros__")
  "Hy-only builtin names.")

;;;; Python Builtins

(defconst hy-font-lock--python-builtins
  '("abs"
    "all"
    "any"
    "ascii"
    "bytes"
    "bin"
    "bool"
    "bytearray"
    "callable"
    "chr"
    "compile"
    "complex"
    "delattr"
    "dict"
    "dir"
    "divmod"
    "enumerate"
    "eval"
    "exec"
    "float"
    "format"
    "frozenset"
    "getattr"
    "globals"
    "hasattr"
    "hash"
    "help"
    "hex"
    "id"
    "input"
    "int"
    "isinstance"
    "issubclass"
    "iter"
    "len"
    "list"
    "locals"
    "map"
    "max"
    "memoryview"
    "min"
    "next"
    "object"
    "oct"
    "open"
    "ord"
    "pow"
    "range"
    "repr"
    "reversed"
    "round"
    "set"
    "setattr"
    "slice"
    "sorted"
    "str"
    "sum"
    "super"
    "tuple"
    "type"
    "vars"
    "--package--" "__package__"
    "--import--" "__import__"
    "--all--" "__all__"
    "--doc--" "__doc__"
    "--name--" "__name__")
  "Builtin names available in Python normally.")

;;;; Constants

(defconst hy-font-lock--constants
  '("True"
    "False"
    "None"
    "Ellipsis"
    "NotImplemented"
    "nil"  ; Provided for those that alias None as nil, not a part of Hy
    )
  "Constant names in Hy.")

;;;; Exceptions

(defconst hy-font-lock--exceptions
  '("ArithmeticError" "AssertionError" "AttributeError" "BaseException"
    "DeprecationWarning" "EOFError" "EnvironmentError" "Exception"
    "FloatingPointError" "FutureWarning" "GeneratorExit" "IOError" "ImportError"
    "ImportWarning" "IndexError" "KeyError" "KeyboardInterrupt" "LookupError"
    "MemoryError" "NameError" "NotImplementedError" "OSError" "OverflowError"
    "PendingDeprecationWarning" "ReferenceError" "RuntimeError" "RuntimeWarning"
    "StopIteration" "SyntaxError" "SyntaxWarning" "SystemError" "SystemExit"
    "TypeError" "UnboundLocalError" "UnicodeDecodeError" "UnicodeEncodeError"
    "UnicodeError" "UnicodeTranslateError" "UnicodeWarning" "UserWarning"
    "VMSError" "ValueError" "Warning" "WindowsError" "ZeroDivisionError"
    "BufferError" "BytesWarning" "IndentationError" "ResourceWarning"
    "TabError")
  "Exception and error names.")

;;;; Definitions

(defconst hy-font-lock--definitions
  '(;; Functions
    "defn" "defn/a"

    ;; Macros
    "defmacro" "defmacro/g!" "defmacro!"

    ;; Tag Macros
    "deftag"

    ;; Defining __main__
    "defmain"

    ;; Multi-methods
    "defmulti" "defmethod")
  "Names in Hy that define functions, macros, etc.")

;;;; Operators

(defconst hy-font-lock--operators
  '("!=" "%" "%=" "&" "&=" "*" "**" "**=" "*=" "+" "+=" "," "-"
    "-=" "/" "//" "//=" "/=" "<" "<<" "<<=" "<=" "=" ">" ">=" ">>" ">>="
    "^" "^=" "|" "|=" "~")
  "Operators in Hy.")

;;;; Special Names

(defconst hy-font-lock--special-names
  '(;; Looping
    "for" "for/a"
    "dfor" "lfor" "sfor"  ; comprehensions
    "loop" "recur"  ; hy.contrib.loop

    ;; Threading
    "->" "->>" "as->"

    ;; Flow control
    "return"
    "if" "if*" "if-not" "lif" "lif-not"
    "else" "unless" "when"
    "break" "continue" "while"
    "cond"
    "do"

    ;; Functional
    "fn" "fn/a"
    "await"
    "yield" "yield-from"
    "with" "with*" "with/a" "with/a*"
    "with-gensyms"

    ;; Error Handling
    "except" "try" "throw" "raise" "catch" "finally" "assert"

    ;; Python builtins (and shadowed calls to builtins)
    "print"
    "not" "and" "or"
    "in" "not-in"

    ;; Namespaces
    "global" "nonlocal"

    ;; Evaluation
    "eval" "eval-and-compile" "eval-when-compile")
  "Special names like compiler stuff to highlight as keywords.")

;;;; Anaphorics

(defconst hy-font-lock--anaphorics
  '("ap-dotimes"
    "ap-each"
    "ap-each-while"
    "ap-filter"
    "ap-first"
    "ap-if"
    "ap-last"
    "ap-map"
    "ap-map-when"
    "ap-reduce"
    "ap-reject")
  "Hy anaphoric contrib keywords.")

;;; Keywords
;;;; Based on Names Lists

(defconst hy-font-lock--kwds-builtins
  (list
   (rx-to-string
    `(: symbol-start
        (or ,@hy-font-lock--hy-builtins
            ,@hy-font-lock--python-builtins
            ,@hy-font-lock--operators
            ,@hy-font-lock--anaphorics)
        symbol-end))

   '(0 font-lock-builtin-face))
  "Hy builtin keywords.")

(defconst hy-font-lock--kwds-constants
  (list
   (rx-to-string
    `(: symbol-start
        (or ,@hy-font-lock--constants)
        symbol-end))

   '(0 font-lock-constant-face))
  "Hy constant keywords.")

(defconst hy-font-lock--kwds-definitions
  (list
   (rx-to-string
    `(: "("
        symbol-start
        (group-n 1 (or ,@hy-font-lock--definitions))
        (1+ space)
        (group-n 2 (1+ word))))

   '(1 font-lock-keyword-face)
   '(2 font-lock-function-name-face nil t))
  "Hy definition keywords.")

(defconst hy-font-lock--kwds-exceptions
  (list
   (rx-to-string
    `(: symbol-start
        (or ,@hy-font-lock--exceptions)
        symbol-end))

   '(0 font-lock-type-face))
  "Hy exception keywords.")

(defconst hy-font-lock--kwds-special-names
  (list
   (rx-to-string
    `(: symbol-start
        (or ,@hy-font-lock--special-names)
        symbol-end))

   '(0 font-lock-keyword-face))
  "Hy special names keywords.")

;;;; Static

(defconst hy-font-lock--kwds-class
  (list
   (rx (group-n 1 "defclass")
       (1+ space)
       (group-n 2 (1+ word)))

   '(1 font-lock-keyword-face)
   '(2 font-lock-type-face))
  "Hy class keywords.")

(defconst hy-font-lock--kwds-decorators
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
  "Hylight the symbol after `#@' or `with-decorator' macros keywords.")

(defconst hy-font-lock--kwds-imports
  (list
   (rx symbol-start
       (or "import"
           "require"
           ":as")
       symbol-end)

   '(0 font-lock-keyword-face))
  "Hy import keywords.")

(defconst hy-font-lock--kwds-self
  (list
   (rx symbol-start
       (group "self")
       (or "." symbol-end))

   '(1 font-lock-keyword-face))
  "Hy self keyword.")

(defconst hy-font-lock--kwds-tag-macros
  (list
   (rx "#"
       ;; #* is unpacking, #@ decorator, #[ bracket str
       (not (any "*"
                 "@"
                 "["
                 ")"
                 space))
       (0+ (syntax word)))

   '(0 font-lock-function-name-face))
  "Hylight tag macros, ie. `#tag-macro', so they stand out.")

;;;; Misc

(defconst hy-font-lock--kwds-anonymous-funcs
  (list
   (rx symbol-start
       (group "%" (1+ digit))
       (or "." symbol-end))

   '(1 font-lock-variable-name-face))
  "Hy '#%(print %1 %2)' styling anonymous variables.")

(defconst hy-font-lock--kwds-func-modifiers
  (list
   (rx symbol-start
       "&"
       (1+ word))

   '(0 font-lock-type-face))
  "Hy '&rest/&kwonly/...' styling.")

(defconst hy-font-lock--kwds-kwargs
  (list
   (rx symbol-start
       ":"
       (1+ word))

   '(0 font-lock-constant-face))
  "Hy ':kwarg' styling.")

(defconst hy-font-lock--kwds-shebang
  (list
   (rx buffer-start
       "#!"
       (0+ not-newline)
       eol)

   '(0 font-lock-comment-face))
  "Hy shebang line.")

(defconst hy-font-lock--kwds-unpacking
  (list
   (rx (or "#*"
           "#**")
       symbol-end)

   '(0 font-lock-keyword-face))
  "Hy #* arg and #** kwarg unpacking keywords.")

(defconst hy-font-lock--kwds-variables
  (list
   (rx symbol-start
       "setv"
       symbol-end
       (1+ space)
       (group (1+ word)))

   '(1 font-lock-variable-name-face))
  "Hylight variable names in setv/def, only first name.")

;;;; Advanced

(defconst hy-font-lock--tag-comment-prefix-rx
  (rx "#_"
      (* " ")
      (group-n 1 (not (any " "))))
  "The regex to match #_ tag comment prefixes.")

(defun hy-font-lock--search-comment-macro (limit)
  "Search for a comment forward stopping at LIMIT."
  (-when-let* ((_ (re-search-forward hy-font-lock--tag-comment-prefix-rx limit t))
               (md (match-data))
               (start (match-beginning 1))
               (state (syntax-ppss start)))
    (if (hy--in-string-or-comment? state)
        (hy-font-lock--search-comment-macro limit)
      (goto-char start)
      (forward-sexp)
      (setf (elt md 3) (point))
      (set-match-data md)
      t)))

(defconst hy-font-lock--kwds-tag-comment-prefix
  (list #'hy-font-lock--search-comment-macro

        '(1 font-lock-comment-face t))
  "Support for higlighting #_(form) the form as a comment.")

;;; Syntactic Face Function
;;;; Utilities

(defun hy-font-lock--string-is-module-docstring? (syntax)
  "Is string SYNTAX specifically a module docstring?"
  (= 1 (hy--syntax->string-start syntax)))

(defun hy-font-lock--string-is-function-docstring? (syntax)
  "Is string SYNTAX specifically a function docstring?"
  (-when-let (inner-symbol (hy--syntax->inner-symbol syntax))
    (when (and (not (s-equals? "defmethod" inner-symbol))
               (s-matches? (rx "def" (not blank)) inner-symbol))
      (let ((start-point (point)))
        (save-excursion
          (hy--goto-inner-sexp syntax)

          (-when-let* ((start (ignore-errors (scan-sexps (point) 3)))
                       (end (ignore-errors (scan-sexps (point) 4))))
            (<= start start-point end)))))))

;;;; Exposes

(defun hy-font-lock-syntactic-face-function (syntax)
  "Return syntactic face function for synatax STATE."
  (if (hy--in-string? syntax)
      (if (or (hy-font-lock--string-is-module-docstring? syntax)
              (hy-font-lock--string-is-function-docstring? syntax))
          font-lock-doc-face
        font-lock-string-face)
    font-lock-comment-face))

;;; Comint Support

(defun hy-font-lock--kwd->comint-kwd (kwd)
  "Converts a `font-lock-keywords' KWD for `comint-mode' input fontification.

This is a rather clever solution to fontifying repl input. I wrote a post
about this idea here: http://www.modernemacs.com/post/comint-highlighting/.

The `comint-snapshot-last-prompt' call within `comint-send' is what makes
this solution tick as future attempts to font-lock prior to the current
prompt will be frozen by comint.

It actually implements comint fontification for arbitrary major modes and have
applied with success to `ielm'."
  (-let (((matcher . match-highlights) kwd))
    `((lambda (limit)
        ;; Matcher can be a function or a regex
        (when ,(if (symbolp matcher)
                   `(,matcher limit)
                 `(re-search-forward ,matcher limit t))

          ;; While the SUBEXP can be anything, this search always can use zero
          (-let ((start (match-beginning 0))
                 ((comint-last-start . comint-last-end) comint-last-prompt)
                 (state (syntax-ppss)))
            (and (> start comint-last-start)
                 ;; Make sure not in comment or string
                 ;; have to manually do this in custom MATCHERs
                 (not (or (nth 3 state) (nth 4 state)))))))

      ,@match-highlights)))

;;; Font Lock Keywords

(defconst hy-font-lock-kwds
  (list hy-font-lock--kwds-builtins
        hy-font-lock--kwds-class
        hy-font-lock--kwds-constants
        hy-font-lock--kwds-definitions
        hy-font-lock--kwds-decorators
        hy-font-lock--kwds-exceptions
        hy-font-lock--kwds-func-modifiers
        hy-font-lock--kwds-imports
        hy-font-lock--kwds-kwargs
        hy-font-lock--kwds-self
        hy-font-lock--kwds-shebang
        hy-font-lock--kwds-special-names
        hy-font-lock--kwds-tag-macros
        hy-font-lock--kwds-unpacking
        hy-font-lock--kwds-variables

        ;; Advanced kwds
        hy-font-lock--kwds-tag-comment-prefix

        ;; Optional kwds
        (when hy-font-lock-highlight-percent-args?
          hy-font-lock--kwds-anonymous-funcs))
  "All Hy font lock keywords.")

(defconst inferior-hy-font-lock-kwds
  (-map #'hy-font-lock--kwd->comint-kwd hy-font-lock-kwds)
  "Comint-compatible version of `hy-font-lock-kwds'.

See `hy-font-lock--kwd->comint-kwd' for details.")

;;; Provide:

(provide 'hy-font-lock)

;;; hy-font-lock.el ends here
