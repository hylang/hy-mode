;;; ERT Tests for Hy-mode  -*- lexical-binding: t -*-

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

(defun hy--assert-indent (text)
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

(ert-deftest test--indent--normal--standard ()
  :tags '(indentation)
  (hy--assert-indent "
(a
  b)
(a b
   c)
(a b c
   d)
"))


(ert-deftest test--indent--normal--empty-lines ()
  :tags '(indentation)
  (hy--assert-indent "
(a

  b)
(a b c

   d)
"))


(ert-deftest test--indent--normal--many-forms ()
  :tags '(indentation)
  (hy--assert-indent "
(a b
   (d e
      f))
(a b c
   d
   e f
   g)
"))

;;;;; Syntax Cases

(ert-deftest test--indent--normal--quote-chars ()
  :tags '(indentation)
  (hy--assert-indent "
(a `b
   c)
(a 'b
   c)
(a ~b
   c)
(a ~@b
   c)
"))


(ert-deftest test--indent--normal--prefix-chars ()
  :tags '(indentation)
  (hy--assert-indent "
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

(ert-deftest test--indent--normal--opens-with-form ()
  :tags '(indentation)
  (hy--assert-indent "
((a b)
  c)
((a b) c
       d)
"))


;; FAIL the no argument case
;; (ert-deftest test--indent--normal--opens-with-tag ()
;;   :tags '(indentation)
;;   (hy--assert-indent
;;    "(#ab"
;;    "  c)"
;;    "(#a b"
;;    "    c)"
;;    ))

;;;; List Likes Indent

(ert-deftest test--indent--lists--brackets ()
  :tags '(indentation)
  (hy--assert-indent "
[a
 b]
[a b
 c]
[\"a\" b
 c]
"))


(ert-deftest test--indent--lists--squiggly-brackets ()
  :tags '(indentation)
  (hy--assert-indent "
{a
 b}
{a b
 c}
{\"a\" b
 c}
"))

;;;; Symbol-likes Indent

(ert-deftest test--indent--symbols--comma ()
  :tags '(indentation)
  (hy--assert-indent "
(,
 a)
(, a
   b)
(, a b
   c)
"))


(ert-deftest test--indent--symbols--vertical-bar ()
  :tags '(indentation)
  (hy--assert-indent "
(|
 a)
(| a
   b)
(| a b
   c)
"))

;;;; Special Forms Indent

(ert-deftest test--indent--special--exact ()
  :tags '(indentation)
  (-let [hy-indent-special-forms
         '(:exact ("foo") :fuzzy ())]
    (hy--assert-indent "
(foo
  a)
(foo a
  b)
(fooo a
      b)
")))


(ert-deftest test--indent--special--fuzzy ()
  :tags '(indentation)
  (-let [hy-indent-special-forms
         '(:exact () :fuzzy ("foo"))]
    (hy--assert-indent "
(foo
  a)
(foo a
  b)
(fooo a
  b)
")))

;;; Bracket String Literals

(ert-deftest test--bracket-strings ()
  :tags '(context-syntax indentation)
  (hy--assert-indent "
(#[[hello
ok]])

(#[delim[hello
ok]delim])

(#[delim[hello

ok]delim])

(#[missing second bracket
   so not a string])
"))
