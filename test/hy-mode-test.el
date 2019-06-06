;;; hy-mode-test.el --- Hy Mode Tests -*- lexical-binding: t -*-

;; `hy-mode' is well-tested.
;; More-or-less all components are tested, including process-based components.

;; See `hy-test.el' for extensions made to Buttercup and other utilities.

;; Covered:
;; - Indentation
;; - Font Locks
;; - Syntax
;;   - Syntax Table Components
;;   - Docstring Detection
;;   - Context Sensitive (Bracket Strings)
;; - Shell support
;;   - Startup/Teardown
;;   - Font Locks
;;   - Sending and getting from running processes
;; - Jedhy (within Jedhy repo, not here)
;; - Misc
;;   - Form extraction

;;; Load hy-test and hy-mode

(progn (require 'f)
       (add-to-list 'load-path (f-parent (f-parent (f-this-file))))
       (require 'hy-test)

       ;; Sets up process-based tests if we have a Hy interpreter to use
       (hy-test--setup-env))

;;; Indentation

(describe "Indentation"
  (before-all (hy-mode--setup-syntax))

  ;; ~~
  ;; NORMAL INDENT
  ;; ~~

  (describe "normal indent"

    (it "opening line has one sexp - indent doesnt carry"
      (expect "
(a
  b
  c)
" :indented))

    (it "opening line has one sexp - indent doesnt carry - with empty lines"
      (expect "
(a

  b

  c)
" :indented))

    (it "opening line has two sexps - indent carries"
      (expect "
(a b
   c)
" :indented))

    (it "opening line has many sexps - indent carries"
      (expect "
(a b c d
   e)
" :indented))

    (it "opening line has many sexps - indent carries - with empty lines"
      (expect "
(a b c d

   e

   f g
   h)
" :indented))

    (it "with nested forms"
      (expect "
(a b (c
       d)
   e (f g h
        i)

   j)
" :indented)))

  ;; ~~
  ;; SYNTAX TABLE
  ;; ~~

  (describe "of syntax table"

    (describe "quote chars"
      (it "backtick"
        (expect "
(a `b
   c)" :indented))

      (it "tick"
        (expect "
(a 'b
   c)" :indented))

      (it "tilde"
        (expect "
(a 'b
   c)" :indented))

      (it "tilde+@ ie. unquote-splice"
        (expect "
(a ~@b
   c)" :indented)))

    (describe "prefix chars"
      (it "dot"
        (expect "
(a .b
   c)" :indented))

      (it "hashtag"
        (expect "
(a #b
   c)" :indented))

      (it "minus sign"
        (expect "
(a -b
   c)" :indented))

      (it "colon ie. keyword"
        (expect "
(a :b
   c)" :indented)))

    (describe "lists"
      (it "list - only sexp on line"
        (expect "
[a
 b]" :indented))

      (it "list - many sexp on line"
        (expect "
[a b
 c d

 e]" :indented))

      (it "list - opening sexp is kwd"
        (expect "
[:a
 b]" :indented)))

    (it "tuple constructor"
      (expect "
(,
  a)
(, a
   b)
(, a b
   c)
" :indented))

    ;; FIXME Not bothering fixing atm because when will anyone ever do this?
    ;; It would make `hy-indent--normal' slightly more obfuscated, a bad thing.
    (xit "matmul operator @"
      (expect "
(@
  a)
(@ a
   b)
" :indented))

    (it "or operator"
      (expect "
(|
  a)
(| a
   b)
(| a b
   c)
" :indented)))

  ;; ~~
  ;; OTHER CASES
  ;; ~~

  (describe "nonstandard cases"
    (it "form opens a form - only sexp on line"
      (expect "
((a b)
  c)
" :indented))

    (it "form opens a form - two+ sexps on line"
      (expect "
((a b) c
 d)
" :indented))

    (it "tag macro opens a form - only sexp on line"
      (expect "
(#a
  c)" :indented))

    (it "tag macro opens a form - two+ sexps on line"
      (expect "
(#a b
    c)" :indented))

    (it "quoted symbol opens a form - only sexp on line"
      (expect "
('a
  c)" :indented))

    (it "quoted symbol opens a form - two+ sexps on line"
      (expect "
('a b
    c)" :indented))

    (it "tilde symbol opens a form - two+ sexps on line"
      (expect "
(~a b
    c)" :indented))

    (it "tilde symbol opens a form - only sexp on line"
      (expect "
(~a
  c)" :indented))

    (it "unquote-spliced symbol opens a form - two+ sexps on line"
      (expect "
(~@a b
     c)" :indented))

    (it "unquote-spliced symbol opens a form - only sexps on line"
      (expect "
(~@a
  c)" :indented))
    )

  ;; ~~
  ;; SPECIAL INDENT RULES
  ;; ~~

  (describe "special indent rules"
    (it "handles exact matches"
      (let ((hy-indent--exactly '("foo")))
        (expect "
(foo
  a)
(foo a
  b)
(fooo a
      b)
" :indented)))

    (it "handles fuzzy matches"
      (let ((hy-indent--fuzzily '("foo")))
        (expect "
(foo
  a)
(foo a
  b)
(fooo a
  b)
" :indented))))

  ;; ~~
  ;; BRACKET STRINGS
  ;; ~~

  (describe "bracket strings never indent"
    (it "all lines have chars"
      (expect "
(#[[a
b]])" :indented))

    (it "not all lines have chars"
      (expect "
(#[[a

b

c]])" :indented))

    (it "has matching delims"
      (expect "
(#[-[a

b

c]-])" :indented))

    (it "has not matching delims"
      (expect "
(#[-+[a

b

c]+-])" :indented))

    (it "only applies when two brackets, not just 1"
      (expect "
(#[no second bracket
   => not a string])
") :indented)))

;;; Font Lock

(describe "Font Lock face application"
  (it "to builtins"
    (expect "«b:+» «b:map» «b:ap-each» map-foo map-do map.foo" :faces))

  (it "to constants"
    (expect "«c:True» Truex xTrue" :faces))

  ;; FIXME
  (xit "to function defs"
    (expect "«k:defn» «f:func» defnx func xdefn func" :faces))

  (it "to exceptions"
    (expect "«t:ValueError» ValueErrorx xValueError" :faces))

  (it "to special forms"
    (expect "«k:->» «k:for» forx xfor for.x x.for" :faces))

  (it "to class definitions"
    (expect "«k:defclass» «t:Klass» [parent]" :faces))

  (it "to tag macro decorator"
    (expect "«t:#@(a-dec» func-def)" :faces))

  (it "to with decorator"
    (expect "(«t:with-decorator a-dec» func-def)" :faces))

  (it "to imports"
    (expect "(«k:import» x [y «k::as» z])" :faces))

  (it "to requires"
    (expect "(«k:require» x [y «k::as» z])" :faces))

  (it "to self kwd"
    (expect "«k:self» x «k:self».x self-x x-self" :faces))

  (it "to tag macros"
    (expect "«f:#tag-x» y" :faces))

  (it "to variables"
    (expect "(«b:setv» «v:foo»)" :faces))

  (it "to #% anon funcs"
    (let ((hy-font-lock-highlight-percent-args? t))
      (expect "«v:%1» «v:%12» «v:%1».foo %foo %1foo «b:%»" :faces)))

  (it "to function &rest and &kwargs"
    (expect "«t:&rest» args «t:&kwargs» kwargs" :faces))

  (it "to kwargs"
    (expect "«c::kwarg-1» foo «c::kwarg-2»" :faces))

  (it "to shebang"
    (expect "«x:#!shebang»\ncode" :faces))

  (it "to #* and #** unpacking sugar"
    (expect "«k:#*» args «k:#**» kwargs" :faces)))

;;; Syntax

(describe "Syntax Table"
  (describe "comments"
    (it "single char"
      (expect "foo «x:; bar»" :faces))

    (it "two+ chars"
      (expect "foo «m:;; »«x:bar»" :faces)))

  (describe "context sensitive"
    (describe "bracket string literals"
      (it "fence set one line"
        (expect "#[«s:[foo]»]" :faces))

      (it "fence set many lines"
        (expect "#[«s:[foo\n\nbar]»]" :faces))

      (it "fence set with quote chars"
        (expect "#[«s:[\"foo\"]»]" :faces))

      (it "fence set with matching delims"
        (expect "#[delim«s:[foo]»delim]" :faces))

      (it "fence set with different delims"
        (expect "#[delim-1«s:[foo]»delim-2]" :faces))

      (it "many bracket strings"
        (expect "#[«s:[foo]»] \nfoo \n #[«s:[foo]»]" :faces))))

  (describe "characters"
    (before-all (set-syntax-table hy-mode-syntax-table))
    (after-each (delete-region (point-min) (point-max)))

    (describe "symbols"
      (it "dots"
        (insert "foo.bar")
        (expect (thing-at-point 'symbol) :to-equal "foo.bar"))

      (it "dashes"
        (insert "foo-bar")
        (expect (thing-at-point 'symbol) :to-equal "foo-bar"))

      (it "hashtag"
        (insert "#foo")
        (expect (thing-at-point 'symbol) :to-equal "#foo")))

    (describe "quotes"
      (it "tick"
        (insert "'foo")
        (expect (thing-at-point 'symbol) :to-equal "foo"))

      (it "backtick"
        (insert "`foo")
        (expect (thing-at-point 'symbol) :to-equal "foo"))

      (it "tilde"
        (insert "~foo")
        (expect (thing-at-point 'symbol) :to-equal "foo"))

      (it "unquote-splice"
        (insert "~@foo")
        (expect (thing-at-point 'symbol) :to-equal "foo")))))

;;; Docstrings

(describe "Docstring Detection"
  (it "distinguishes module docstrings"
    (expect "«d:\"foo\"»" :faces)
    (expect " «s:\"foo\"»" :faces))

  (it "strings identify parent form"
    (expect "(«k:defn» «f:foo» [] «d:\"bar\"»)" :faces)
    (expect "(«k:defn» «f:foo» [] [«s:\"bar\"»])" :faces))

  (it "has only first string of a defn as the docstring"
    (expect "(«k:defn» «f:foo» [] «d:\"bar\"» «s:\"baz\"»)" :faces)))

;;; Form Captures

(describe "Form Capturing"
  (before-all (hy-mode--setup-syntax))
  (after-each (delete-region (point-min) (point-max)))

  (describe "current form"
    (it "captures a simple parenthetical form"
      (setq text "(foo bar)")
      (insert text)
      (backward-char)

      (expect (hy--current-form-string) :to-equal (s-concat text "\n")))

    (it "captures a simple list form"
      (setq text "[foo bar]")
      (insert text)
      (backward-char)

      (expect (hy--current-form-string) :to-equal (s-concat text "\n")))

    (it "captures nested forms"
      (setq text "[foo (foo bar) bar]")
      (setq paren-start 6)
      (insert text)
      (backward-char)

      (expect (hy--current-form-string) :to-equal (s-concat text "\n"))
      (goto-char (1+ paren-start))
      (expect (hy--current-form-string) :to-equal (s-concat "(foo bar)" "\n"))))

  (describe "last sexp"
    (it "captures a simple parenthetical form"
      (setq text "(foo bar)")
      (insert text)

      (expect (hy--last-sexp-string) :to-equal (s-concat "(foo bar)" "\n")))

    (it "captures nested forms"
      (setq text "[foo (foo bar) bar]")
      (setq paren-end 14)
      (insert text)

      (expect (hy--last-sexp-string) :to-equal (s-concat text "\n"))
      (goto-char (1+ paren-end))
      (expect (hy--last-sexp-string) :to-equal (s-concat "(foo bar)" "\n")))))

;;; Shell

(nt-shell-describe "Shell"
  (describe "startup"
    (before-all (hy-test--run-hy))
    (after-all (hy-shell--kill))

    (it "sets the name and switches"
      (expect (buffer-name) :to-equal hy-shell--buffer-name))

    (it "sets inferior-hy-mode"
      (expect major-mode :to-be 'inferior-hy-mode))

    (it "starts the process and associates with the buffer"
      (expect (process-live-p (hy-shell--current-process)))))

  (describe "teardown"
    (before-each (hy-test--run-hy))
    (after-each (hy-shell--kill))

    (it "kills the buffer and process"
      (expect (hy-shell--live?))
      (expect (hy-shell--kill))
      (expect (hy-shell--live?) :nil)
      (expect (hy-shell--kill) :nil)))

  ;; No need to copy over all the different face tests, as these are built as
  ;; transforms of those keywords, rather than being new keywords entirely.
  (describe "font locking"
    (it "fontifies shell input"
      (expect "(«k:defn» «f:foo» [] «d:\"bar\"»)" :shell-faces)))

  (describe "sending redirected"
    (before-all (hy-test--run-hy))
    (after-all (hy-shell--kill))

    (it "extracts output properly"
      ;; Depends on the interpreter args, improving this
      (expect (hy-shell--redirect-send "(+ 1 2)") :to-equal
              "1 + 2

3"))))
