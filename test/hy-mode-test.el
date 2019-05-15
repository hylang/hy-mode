;;; hy-mode-test.el --- Hy Mode Tests -*- lexical-binding: t -*-

;; `hy-mode' is well-tested for a major-mode.
;; See `hy-test.el' for extensions made to Buttercup to support testing.

;; Not all tests are passing - marked with `xit' and FIXME comments

;; Covered:
;; - Indentation
;; - Font Locks
;; - Syntax
;;   - Syntax Table Components
;;   - Docstring Detection
;;   - Context Sensitive (Bracket Strings)
;; - All IDE features (tested within Jedhy, not this repo)

;; Not Covered:
;; - All process-based stuff and Jedhy interaction
;; - Form and other text extraction for sending to shell
;; - Some misc stuff

;;; Load hy-test and hy-mode

(progn (require 'f)
       (add-to-list 'load-path (f-parent (f-parent (f-this-file))))
       (require 'hy-test))

;;; Indentation

(describe "Indentation"
  (before-all (hy--mode-setup-syntax))

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

    ;; FIXME
    (xit "tuple constructor"
      (expect "
(,
 a)
(, a
   b)
(, a b
   c)
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
  ;; NONSTANDARD CASES
  ;; ~~

  (describe "nonstandard cases"
    (it "form opens a form - only sexp on line"
      (expect "
((a b)
 c)
" :indented))

    ;; FIXME
    (xit "form opens a form - two+ sexps on line"
      (expect "
((a b) c
       d)
" :indented))

    (it "tag macro opens a form - only sexp on line"
      (expect "
(#a
 c)" :indented))

    ;; FIXME
    (xit "tag macro opens a form - two+ sexps on line"
      (expect "
(#a b
    c)" :indented)))

  ;; ~~
  ;; SPECIAL INDENT RULES
  ;; ~~

  (describe "special indent rules"
    (it "handles exact matches"
      (let ((hy-indent-special-forms '(:exact ("foo") :fuzzy ())))
        (expect "
(foo
  a)
(foo a
  b)
(fooo a
      b)
" :indented)))

    (it "handles fuzzy matches"
      (let ((hy-indent-special-forms '(:exact () :fuzzy ("foo"))))
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
    (expect "«b:+» «b:flatten» «b:ap-each» map-foo map-do map.foo" :faces))

  (it "to constants"
    (expect "«c:True» Truex xTrue" :faces))

  ;; FIXME
  (xit "to function defs"
    (expect "«k:defn» «f:func» defnx func xdefn func" :faces))

  (it "to exceptions"
    (expect "«t:ValueError» ValueErrorx xValueError" :faces))

  (it "to special forms"
    (expect "«k:->» «k:for» forx xfor for.x x.for" :faces))

  ;; FIXME - Think this is deprecated?
  (xit "to aliases"
    (expect "«k:defmacro-alias» [«f:arg1 arg2 arg3»]" :faces))

  (it "to class definitions"
    (expect "«k:defclass» «t:Klass» [parent]" :faces))

  (it "to special forms"
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
    (before-all (hy--mode-setup-syntax))
    (after-each (delete-region (point-min) (point-max)))

    (describe "symbols"
      (it "dots"
        (insert "foo.bar")
        (expect (thing-at-point 'symbol) :to-equal "foo.bar"))

      (it "dashes"
        (insert "foo-bar")
        (expect (thing-at-point 'symbol) :to-equal "foo-bar"))

      ;; FIXME What did I decide on again?
      (xit "hashtag"
        (insert "#foo")
        (expect (thing-at-point 'symbol) :to-equal "#foo")))

    (describe "quotes"
      (it "tick"
        (insert "'foo")
        (expect (thing-at-point 'symbol) :to-equal "foo"))

      (it "backtick"
        (insert "`foo")
        (expect (thing-at-point 'symbol) :to-equal "foo"))

      ;; FIXME
      (xit "tilde"
        (insert "~foo")
        (expect (thing-at-point 'symbol) :to-equal "foo"))

      ;; FIXME
      (xit "unquote-splice"
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

  ;; FIXME - Not Implemented, known issue
  (it "has only first string of a defn as the docstring"
    (expect "(«k:defn» «f:foo» [] «d:\"bar\"» «s:\"baz\"»)" :faces)))
