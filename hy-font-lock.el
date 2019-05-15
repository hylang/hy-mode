;;; hy-font-lock.el --- Font Locks -*- lexical-binding: t -*-

;; Copyright © 2013 Julien Danjou <julien@danjou.info>
;;           © 2017 Eric Kaschalk <ekaschalk@gmail.com>
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

;; Font lock definitions and setup for `hy-mode'

;; This file is long but organized.

;;; Hy Builtins

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

;;; Python Builtins

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

;;; Constants

(defconst hy-font-lock--constants
  '("True"
    "False"
    "None"
    "Ellipsis"
    "NotImplemented"
    "nil"  ; Provided for those that alias None as nil, not a part of Hy
    )
  "Constant names in Hy.")

;;; Exceptions

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

;;; Definitions

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

;;; Operators

(defconst hy-font-lock--operators
  '("!=" "%" "%=" "&" "&=" "*" "**" "**=" "*=" "+" "+=" "," "-"
    "-=" "/" "//" "//=" "/=" "<" "<<" "<<=" "<=" "=" ">" ">=" ">>" ">>="
    "^" "^=" "|" "|=" "~")
  "Operators in Hy.")

;;; Special Names

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

;;; Anaphorics

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

;;; Provide:

(provide 'hy-font-lock)
