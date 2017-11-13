;;; Tests for Hy-mode  -*- lexical-binding: t -*-

(require 'ert)
(require 'hy-mode)

;; - TESTED -
;; Indentation
;; Bracket string literals

;; - REMAINING -
;; Shell
;; Font-locks
;; Eldoc
;; Autocompletion
;; font-lock-syntactic-face-function

;;; Assertions

(defun hy--assert-indented (text)
  "Assert indenting the left-trimmed version of TEXT matches TEXT."
  (with-temp-buffer
    (-let [no-indents
           (->> text s-lines (-map 's-trim-left) (s-join "\n"))]
      (hy-mode)
      (insert no-indents)
      (indent-region (point-min) (point-max))

      (-> (buffer-substring-no-properties (point-min) (point-max))
         (s-equals? text)
         should))))

;;; Indentation Tests
;;;; Normal Indent
;;;;; Standard Cases

(ert-deftest indent::normal-indent ()
  :tags '(indentation)
  (hy--assert-indented "
(a
  b)
(a b
   c)
(a b c
   d)
"))


(ert-deftest indent::with-empty-lines ()
  :tags '(indentation)
  (hy--assert-indented "
(a

  b)
(a b c

   d)
"))


(ert-deftest indent::with-many-forms ()
  :tags '(indentation)
  (hy--assert-indented "
(a b
   (d e
      f))
(a b c
   d
   (e f)
   g)
"))

;;;;; Syntax Cases

(ert-deftest indent::quote-chars ()
  :tags '(indentation)
  (hy--assert-indented "
(a `b
   c)
(a 'b
   c)
(a ~b
   c)
(a ~@b
   c)
"))


(ert-deftest indent::prefix-chars ()
  :tags '(indentation)
  (hy--assert-indented "
(a .b
   c)
(a #b
   c)
(a -b
   c)
(a :b
   c)
"))

;;;;; Special Form Opener Cases

(ert-deftest indent::form-as-opener ()
  :tags '(indentation)
  (hy--assert-indented "
((a b)
  c)
((a b) c
       d)
"))


;; FAIL Expected
;; (#a
;;   c)
;; Actual has 1+ indentation
;; (#a
;;    b)
(ert-deftest indent::tag-as-opener ()
  :tags '(indentation)
  (hy--assert-indented "
(#a b
    c)
"))

;;;; Literals Indent

(ert-deftest indent::list-literals ()
  :tags '(indentation)
  (hy--assert-indented "
[a
 b]
[a b
 c]
[\"a\" b
 c]
"))


(ert-deftest indent::dict-literals ()
  :tags '(indentation)
  (hy--assert-indented "
{a
 b}
{a b
 c}
{\"a\" b
 c}
"))

;;;; Symbol-likes Indent

(ert-deftest indent::tuple-constructor ()
  :tags '(indentation)
  (hy--assert-indented "
(,
 a)
(, a
   b)
(, a b
   c)
"))


(ert-deftest indent::pipe-or-operator ()
  :tags '(indentation)
  (hy--assert-indented "
(|
 a)
(| a
   b)
(| a b
   c)
"))

;;;; Special Forms Indent

(ert-deftest indent::special-exact-forms ()
  :tags '(indentation)
  (-let [hy-indent-special-forms
         '(:exact ("foo") :fuzzy ())]
    (hy--assert-indented "
(foo
  a)
(foo a
  b)
(fooo a
      b)
")))


(ert-deftest indent::special-fuzzy-forms ()
  :tags '(indentation)
  (-let [hy-indent-special-forms
         '(:exact () :fuzzy ("foo"))]
    (hy--assert-indented "
(foo
  a)
(foo a
  b)
(fooo a
  b)
")))

;;; Bracket String Literals

(ert-deftest context-sensitive-syntax::bracket-strings ()
  :tags '(context-syntax indentation)
  (hy--assert-indented "
(#[[hello
ok]])

(#[delim[hello
ok]delim])

(#[delim-a[hello

ok]delim-b])

(#[missing second bracket
   so not a string])
"))
