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

(require 'cl)
(require 'dash)
(require 'dash-functional)
(require 's)

(defgroup hy-mode nil
  "A mode for Hy"
  :prefix "hy-mode-"
  :group 'applications)

;;; Configuration
;;;; Inferior shell

(defconst hy-shell-interpreter "hy"
  "Default Hy interpreter name.")

(defconst hy-shell-interpreter-args "--spy"
  "Default arguments for Hy interpreter.")

;; This command being phased out in favor of `run-hy'
(defcustom hy-mode-inferior-lisp-command "hy"
  "The command used by `inferior-lisp-program'."
  :type 'string
  :group 'hy-mode)

(defconst hy-shell-use-control-codes? nil
  "Append `--control-codes' flag to `hy-shell-interpreter-args'?

Requires recent version of Hy.")

(defconst hy-shell-spy-delim ""
  "If using `--spy' interpreter arg then delimit spy ouput.")

;;;; Indentation

(defconst hy-indent-special-forms
  '(:exact
    ("when" "unless"
     "for" "for*"
     "while"
     "except" "catch")

    :fuzzy
    ("def"
     "with"
     "fn"
     "lambda"))
  "Special forms to always indent following line by +1.")

;;; Syntax Table

(defconst hy-mode-syntax-table
  (-let [table
         (copy-syntax-table lisp-mode-syntax-table)]
    (modify-syntax-entry ?\{ "(}" table)
    (modify-syntax-entry ?\} "){" table)
    (modify-syntax-entry ?\[ "(]" table)
    (modify-syntax-entry ?\] ")[" table)

    ;; Quote characters are prefixes
    (modify-syntax-entry ?\~ "'" table)
    (modify-syntax-entry ?\@ "'" table)

    ;; "," is a symbol in Hy, namely the tuple constructor
    (modify-syntax-entry ?\, "_ p" table)

    ;; "|" is a symbol in hy, naming the or operator
    (modify-syntax-entry ?\| "_ p" table)

    ;; "#" is a tag macro, we include # in the symbol
    (modify-syntax-entry ?\# "_ p" table)

    table)
  "Hy modes syntax table.")

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
       (not (any "*" "@" "["))  ; #* is unpacking, #@ decorator, #[ bracket str
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
;;;; Utilities

;; Aliases for `parse-partial-sexp' value
(defun hy--sexp-inermost-char (state)
  (nth 1 state))
(defun hy--start-of-last-sexp (state)
  (nth 2 state))
(defun hy--in-string? (state)
  (nth 3 state))
(defun hy--start-of-string (state)
  (nth 8 state))

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
  (member (char-after pos) '(?\, ?\|)))

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
  (or (-contains? '(?\[ ?\{) (char-after))
      (not (looking-at (rx anything  ; Skips form opener
                           (or (syntax symbol) (syntax word)))))))

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

;;; Bracket String Literals

(defun hy--match-bracket-string (limit)
  "Search forward for a bracket string literal."
  (re-search-forward
   (rx "#["
       (0+ not-newline)
       "["
       (group (1+ (not (any "]"))))
       "]"
       (0+ not-newline)
       "]")
   limit
   t))

(defun hy-syntax-propertize-function (start end)
  "Implements context sensitive syntax highlighting beyond `font-lock-keywords'.

In particular this implements bracket string literals.
START and END are the limits with which to search for bracket strings passed
and determined by `font-lock-mode' internals when making an edit to a buffer.
"
  (save-excursion
    (goto-char start)

    ;; Start goes to current line, need to go to start of #[[ block
    (when (nth 1 (syntax-ppss))  ; when inermost-char go to [ => [ => #
      (goto-char (- (hy--sexp-inermost-char (syntax-ppss)) 2)))

    (while (hy--match-bracket-string end)
      (put-text-property (1- (match-beginning 1)) (match-beginning 1)
                         'syntax-table (string-to-syntax "|"))

      (put-text-property (match-end 1) (1+ (match-end 1))
                         'syntax-table (string-to-syntax "|")))))

;;; Font Lock Syntactics

(defun hy--string-in-doc-position? (state)
  "Is STATE within a docstring?"
  (if (= 1 (hy--start-of-string state))  ; Identify module docstring
      t
    (-when-let* ((first-sexp (hy--sexp-inermost-char state))
                 (function (save-excursion
                             (goto-char (1+ first-sexp))
                             (thing-at-point 'symbol))))
      (s-matches? (rx "def" (not blank)) function))))  ; "def"=="setv"

(defun hy-font-lock-syntactic-face-function (state)
  "Return syntactic face function for the position represented by STATE.
STATE is a `parse-partial-sexp' state, and the returned function is the
Lisp font lock syntactic face function. String is shorthand for either
a string or comment."
  (if (hy--in-string? state)
      (if (hy--string-in-doc-position? state)
          font-lock-doc-face
        font-lock-string-face)
    font-lock-comment-face))

;;; Shell Integration
;;;; Configuration

(defconst hy-shell-buffer-name "Hy"
  "Default buffer name for Hy interpreter.")

(defconst hy-shell-internal-buffer-name "Hy Internal"
  "Default buffer name for the internal Hy process.")

(defvar hy-shell-buffer nil
  "The current shell buffer for Hy.")

(defconst hy--spy-delim-uuid "#cbb4fcbe-b6ba-4812-afa3-4a5ac7b20501"
  "UUID denoting end of python block in `--spy --control-categories' output")

(setq hy-shell-font-lock-enable t)

;;;; Shell buffer utilities

(defun hy-shell-format-process-name (proc-name &optional internal)
  "Format a PROC-NAME with closing astericks."
  (->> proc-name (s-prepend "*") (s-append "*")))

(defun hy-shell-get-process ()
  "Get the Hy process corresponding to `hy-shell-buffer-name'."
  (-> hy-shell-buffer-name
     hy-shell-format-process-name
     get-buffer-process))

(defun hy-shell-get-internal-process ()
  "Get the Hy internal process corresponding to `hy-shell-internal-buffer-name'."
  (-> hy-shell-internal-buffer-name
     (hy-shell-format-process-name t)
     get-buffer-process))

(defun hy-shell-get-or-create-buffer ()
  "Get or create a `hy-shell-buffer' for current inferior process."
  (if hy-shell-buffer
      hy-shell-buffer
    (hy-shell-with-shell-buffer
     (-let [process-name
            (-> (current-buffer) get-buffer-process process-name)]
       (generate-new-buffer process-name)))))

(defun hy-shell-buffer? ()
  "Is `hy-shell-buffer' set and does it exist?"
  (and hy-shell-buffer
       (buffer-live-p hy-shell-buffer)))

(defun hy-shell-kill-buffer ()
  "Kill a `hy-shell-buffer'."
  (when (hy-shell-buffer?)
    (kill-buffer hy-shell-buffer)
    (when (derived-mode-p 'inferior-hy-mode)
      (setq hy-shell-buffer nil))))

;;;; Shell macros

(defmacro hy-shell-with-shell-buffer (&rest forms)
  "Execute FORMS in the shell buffer."
  (-let [shell-process
         (gensym)]
    `(-let [,shell-process
            (hy-shell-get-process)]
       (with-current-buffer (process-buffer ,shell-process)
         ,@forms))))

(defmacro hy-shell-with-font-locked-shell-buffer (&rest forms)
  "Execute FORMS in the shell buffer with font-lock on."
  `(hy-shell-with-shell-buffer
    (save-current-buffer
      (unless (hy-shell-buffer?)
        (setq hy-shell-buffer (hy-shell-get-or-create-buffer)))

      (set-buffer hy-shell-buffer)

      (unless (font-lock-mode)
        (font-lock-mode 1))
      (unless (derived-mode-p 'hy-mode)
        (hy-mode))

      ,@forms)))

;;;; Font locking

(defun hy-shell-post-command-hook ()
  "Fontify the current line in `hy-shell-buffer' for `post-command-hook'."
  (-let [(_ . prompt-end)
         comint-last-prompt]
    (when (and prompt-end
               (> (point) prompt-end)
               (-> (current-buffer) get-buffer-process process-live-p))
      (let* ((input (buffer-substring-no-properties
                     prompt-end (point-max)))
             (deactivate-mark nil)
             (start-pos prompt-end)
             (buffer-undo-list t)
             (font-lock-buffer-pos nil)
             (replacement
              (hy-shell-with-font-locked-shell-buffer
               (delete-region (line-beginning-position) (point-max))
               (setq font-lock-buffer-pos (point))
               (insert input)
               (funcall 'font-lock-ensure)
               (buffer-substring font-lock-buffer-pos
                                 (point-max))))
             (replacement-length (length replacement))
             (i 0))
        ;; Inject text properties to get input fontified.
        (while (/= i replacement-length)
          (let* ((plist (text-properties-at i replacement))
                 (plist (-let [face
                               (plist-get plist 'face)]
                          (if (not face)
                              plist
                            (plist-put plist 'face nil)
                            (plist-put plist 'font-lock-face face))))
                 (next-change (or (next-property-change i replacement)
                                  replacement-length))
                 (text-begin (+ start-pos i))
                 (text-end (+ start-pos next-change)))

            (set-text-properties text-begin text-end plist)
            (setq i next-change)))))))

(defun hy-shell-font-lock-turn-on ()
  "Turn on fontification of current line for hy shell."
  (hy-shell-with-shell-buffer
   (hy-shell-kill-buffer)
   (setq-local hy-shell-buffer nil)
   (add-hook 'post-command-hook
             'hy-shell-post-command-hook nil 'local)
   (add-hook 'kill-buffer-hook
             'hy-shell-kill-buffer nil 'local)))

(defun hy-shell-faces-to-font-lock-faces (text)
  "Goes through text setting 'face properties to 'font-lock-face."
  (-let [pos 0]
    (while (setq next (next-single-property-change pos 'face text))
      (put-text-property pos next 'font-lock-face
                         (get-text-property pos 'face text) text)
      (setq pos next))
    (add-text-properties 0 (length text) '(fontified t) text)
    text))

(defun hy-shell-font-lock-spy-output (string)
  "Applies font-locking to hy outputted python blocks when `--spy' is enabled."
  (with-temp-buffer
    (if (s-contains? hy--spy-delim-uuid string)
        (-let ((python-indent-guess-indent-offset
                nil)
               ((python-block hy-output)
                (s-split hy--spy-delim-uuid string)))
          (python-mode)
          (insert python-block)
          (font-lock-default-fontify-buffer)
          (-> (buffer-string)
             hy-shell-faces-to-font-lock-faces
             s-chomp
             (s-concat hy-shell-spy-delim hy-output)))
      string)))

;;;; Send strings

(defvar hy-shell--output-filter-in-progress nil
  "Whether we are waiting for output in `hy-shell-send-string-no-output'.")

(defun hy-shell-comint-end-of-output-p (string)
  "Return non-nil if STRING ends with the input prompt."
  (string-match (rx "=> " string-end) string))

(defun hy-shell-output-filter (string)
  "If STRING ends with input prompt then set filter in progress done."
  (when (hy-shell-comint-end-of-output-p string)
    (setq hy-shell--output-filter-in-progress nil))
  "\n=> ")

(defun hy-shell-send-string-no-output (string &optional process)
  "Send STRING to PROCESS and inhibit output. Return the output."
  (let ((process
         (or process (hy-shell-get-process)))
        (comint-preoutput-filter-functions
         '(hy-shell-output-filter))
        (hy-shell--output-filter-in-progress
         t))
    (comint-send-string process string)
    (while hy-shell--output-filter-in-progress
      (accept-process-output process))))

;;;; Shell creation

(defun hy-shell-make-comint (cmd proc-name &optional show internal)
  (save-excursion
    (-let [proc-buffer-name
           (hy-shell-format-process-name proc-name internal)]
      (unless (comint-check-proc proc-buffer-name)
        (-let* ((cmdlist (split-string-and-unquote cmd))
                ((interpreter . args) cmdlist)
                (buffer (apply 'make-comint-in-buffer
                               proc-name proc-buffer-name interpreter nil
                               args))
                (process (get-buffer-process buffer)))
          (with-current-buffer buffer
            (inferior-hy-mode))
          (when show
            (display-buffer buffer))
          (if internal
              (set-process-query-on-exit-flag process nil)
            (setq hy-shell-buffer buffer))))
      proc-buffer-name)))

(defun hy-shell-calculate-interpreter-args ()
  "Calculate `hy-shell-interpreter-args' based on `--spy' flag."
  (if (and hy-shell-use-control-codes?
           (s-contains? "--spy" hy-shell-interpreter-args))
      (s-concat hy-shell-interpreter-args " --control-codes")
    hy-shell-interpreter-args))

(defun hy-shell-calculate-command (&optional internal)
  "Calculate the string used to execute the inferior Hy process."
  (format "%s %s"
          (shell-quote-argument hy-shell-interpreter)
          (if internal
              ""
            (hy-shell-calculate-interpreter-args))))

(defun run-hy (&optional cmd)
  "Run an inferior Hy process.

CMD defaults to the result of `hy-shell-calculate-command'."
  (interactive)
  (-> (or cmd
         (hy-shell-calculate-command))
     (hy-shell-make-comint hy-shell-buffer-name t)
     get-buffer-process))

(defun run-hy-internal ()
  "Start an inferior hy process in the background for autocompletion."
  (when (and (not (hy-shell-get-internal-process))
             (executable-find "hy"))
    (-let [hy-shell--font-lock-enable
           nil]
      (-> (hy-shell-calculate-command t)
         (hy-shell-make-comint hy-shell-internal-buffer-name nil t)
         get-buffer-process))))

;;; Autocompletion

(defun hy-comint-redirect-results-list-from-process (process command regexp regexp-group)
  "Execute `comint-redirect-results-list-from-process' with timeout for company."
  (let ((output-buffer " *Comint Redirect Work Buffer*")
        results)
    (with-current-buffer (get-buffer-create output-buffer)
      (erase-buffer)
      (comint-redirect-send-command-to-process command
                                               output-buffer process nil t)
      ;; Wait for the process to complete
      (set-buffer (process-buffer process))
      (while (and (null comint-redirect-completed)
                  (accept-process-output process nil 100 t)))
      ;; Collect the output
      (set-buffer output-buffer)

      (goto-char (point-min))
      ;; Skip past the command, if it was echoed
      (and (looking-at command)
           (forward-line))
      (while (and (not (eobp))
                  (re-search-forward regexp nil t))
        (push (buffer-substring-no-properties
               (match-beginning regexp-group)
               (match-end regexp-group))
              results))
      (nreverse results))))

(defun hy-get-matches (&optional str)
  "Return matches for STR."
  (when (hy-shell-get-internal-process)
    (hy-comint-redirect-results-list-from-process
     (hy-shell-get-internal-process)
     (concat
      ;; Currently always importing, determine if can do at start
      "(do (import [hy.completer [Completer]]) (setv completer (Completer))"
      "(completer."
      (cond ((s-starts-with? "#" str)  ; Hy not recording tag macros atm
             "tag-matches")

            ((s-contains? "." str)
             "attr-matches")

            (t
             "global-matches"))
      " \"" str "\")"
      ")")
     (rx "'"
         (group (1+ (or word (any "!@#$%^&*-_+=?~`'<>,.\|"))))
         "'"
         (any "," "]"))
     1)))

(defun hy-company (command &optional arg &rest ignored)
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'hy-company))
    (prefix (company-grab-symbol))
    (candidates (cons :async
                      (lexical-let ((prfx arg))
                        (lambda (cb)
                          (let ((res (hy-get-matches prfx)))
                            (funcall cb res))))))
    ;; (meta (format "This value is named %s" arg))
    ))

;;; hy-mode and inferior-hy-mode
;;;; Hy-mode setup

(defun hy--mode-setup-font-lock ()
  (setq font-lock-defaults
        '(hy-font-lock-kwds
          nil nil
          (("+-*/.<>=!?$%_&~^:@" . "w"))  ; syntax alist
          nil
          (font-lock-mark-block-function . mark-defun)
          (font-lock-syntactic-face-function  ; Differentiates (doc)strings
           . hy-font-lock-syntactic-face-function))))

(defun hy--mode-setup-syntax ()
  ;; Bracket string literals require context sensitive highlighting
  (setq-local syntax-propertize-function 'hy-syntax-propertize-function)

  ;; AutoHighlightSymbol needs adjustment for symbol recognition
  (setq-local ahs-include "^[0-9A-Za-z/_.,:;*+=&%|$#@!^?-~\-]+$")

  ;; Lispy comment syntax
  (setq-local comment-start ";")
  (setq-local comment-start-skip
              "\\(\\(^\\|[^\\\\\n]\\)\\(\\\\\\\\\\)*\\)\\(;+\\|#|\\) *")
  (setq-local comment-add 1)

  ;; Lispy indent with hy-specialized indentation
  (setq-local indent-tabs-mode nil)
  (setq-local indent-line-function 'lisp-indent-line)
  (setq-local lisp-indent-function 'hy-indent-function))

(defun hy--mode-setup-smartparens ()
  "Setup smartparens, if active, pairs for Hy."
  (when (fboundp 'sp-local-pair)
    (sp-local-pair '(hy-mode) "`" "`" :actions nil)
    (sp-local-pair '(hy-mode) "'" "'" :actions nil)
    (sp-local-pair '(inferior-hy-mode) "`" "" :actions nil)
    (sp-local-pair '(inferior-hy-mode) "'" "" :actions nil)))

(defun hy--mode-setup-inferior ()
  (setenv "PYTHONIOENCODING" "UTF-8")

  (setq-local inferior-lisp-program hy-mode-inferior-lisp-command)
  (setq-local inferior-lisp-load-command
              (concat "(import [hy.importer [import-file-to-module]])\n"
                      "(import-file-to-module \"__main__\" \"%s\")\n"))

  (run-hy-internal)
  (add-hook 'pyvenv-post-activate-hooks 'run-hy-internal nil t))

;;;; Inferior-hy-mode setup

(defun hy--inferior-mode-setup ()
  (setq mode-line-process '(":%s"))
  (setq-local indent-tabs-mode nil)
  (setq-local comint-prompt-read-only nil)

  ;; So errors are highlighted according to colorama python package
  (ansi-color-for-comint-mode-on)
  (setq-local comint-output-filter-functions
              '(ansi-color-process-output))

  ;; Don't startup font lock for internal processes
  (when hy-shell-font-lock-enable
    (setq-local comint-preoutput-filter-functions
                `(xterm-color-filter hy-shell-font-lock-spy-output))
    (hy-shell-font-lock-turn-on))

  ;; Fixes issue with "=>", no side effects from this advice
  (advice-add 'comint-previous-input :before
              (lambda (&rest args) (setq-local comint-stored-incomplete-input ""))))

;;;; Core

(add-to-list 'auto-mode-alist '("\\.hy\\'" . hy-mode))
(add-to-list 'interpreter-mode-alist '("hy" . hy-mode))

;;;###autoload
(define-derived-mode inferior-hy-mode comint-mode "Inferior Hy"
  "Major mode for Hy inferior process."
  (hy--inferior-mode-setup))

;;;###autoload
(define-derived-mode hy-mode prog-mode "Hy"
  "Major mode for editing Hy files."
  (hy--mode-setup-font-lock)
  (hy--mode-setup-smartparens)
  (hy--mode-setup-syntax)
  (hy--mode-setup-inferior))

;;; Keybindings
;;;; Utilities

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

;;;###autoload
(defun hy-shell-start-or-switch-to-shell ()
  (interactive)
  (if (hy-shell-buffer?)
      (switch-to-buffer-other-window
       (hy-shell-get-or-create-buffer))
    (run-hy)))

;;;###autoload
(defun hy-shell-eval-buffer ()
  "Send the buffer to the shell, inhibiting output."
  (interactive)
  (-let [text
         (buffer-string)]
    (unless (hy-shell-buffer?)
      (hy-shell-start-or-switch-to-shell))
    (hy-shell-with-shell-buffer
     (hy-shell-send-string-no-output text))))

;;;###autoload
(defun hy-shell-eval-region ()
  "Send highlighted region to shell, inhibiting output."
  (interactive)
  (when (and (region-active-p)
             (not (region-noncontiguous-p)))
    (-let [text
           (buffer-substring (region-beginning) (region-end))]
      (unless (hy-shell-buffer?)
        (hy-shell-start-or-switch-to-shell))
      (hy-shell-with-shell-buffer
       (hy-shell-send-string-no-output text)))))

;;;; Keybindings

(set-keymap-parent hy-mode-map lisp-mode-shared-map)
(define-key hy-mode-map (kbd "C-M-x")   'lisp-eval-defun)
(define-key hy-mode-map (kbd "C-x C-e") 'lisp-eval-last-sexp)
(define-key hy-mode-map (kbd "C-c C-z") 'switch-to-lisp)
(define-key hy-mode-map (kbd "C-c C-l") 'lisp-load-file)

(define-key hy-mode-map (kbd "C-c C-e") 'hy-shell-start-or-switch-to-shell)
(define-key hy-mode-map (kbd "C-c C-b") 'hy-shell-eval-buffer)

(define-key hy-mode-map (kbd "C-c C-t") 'hy-insert-pdb)
(define-key hy-mode-map (kbd "C-c C-S-t") 'hy-insert-pdb-threaded)

(provide 'hy-mode)

;;; hy-mode.el ends here
