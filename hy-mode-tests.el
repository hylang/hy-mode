;;; Tests for Hy-mode  -*- lexical-binding: t -*-

(require 'hy-mode)

(require 'ert)
(require 'faceup)

(require 'dash)
(require 's)

;; - TESTED -
;; Indentation
;; Font-locks
;; Syntax
;; Context Sensitive Syntax
;; Docstring Detection
;; Keybindings

;; - REMAINING -
;; Shell
;; Eldoc
;; Autocompletion
;; Shift-k documentation lookup

;;; Utilities

(defmacro hy-with-hy-mode (&rest forms)
  "Execute FORMS in a temporary `hy-mode' buffer."
  `(with-temp-buffer
     (hy-mode)
     ,@forms))


(defun hy--font-lock-test (text)
  "Entry to test faces of TEXT markedup as faceup, disabling minor modes faces.

See `faceup-face-short-alist' for faceup's face aliases."
  (when (fboundp 'rainbow-delimiters-mode-disable)
    (advice-add 'hy-mode :after 'rainbow-delimiters-mode-disable))
  (prog1
      (faceup-test-font-lock-string 'hy-mode text)
    (advice-remove 'hy-mode 'rainbow-delimiters-mode-disable)))
(faceup-defexplainer hy--font-lock-test)

;;; Assertions

(defun s-assert (string1 string2)
  "Combine `should' and `s-equals?'."
  (should (s-equals? string1 string2)))


(defun hy--assert-indented (text)
  "Assert indenting the left-trimmed version of TEXT matches TEXT."
  (hy-with-hy-mode
   (-let [no-indents
          (->> text s-lines (-map 's-trim-left) (s-join "\n"))]
     (insert no-indents)
     (indent-region (point-min) (point-max))

     (s-assert (buffer-substring-no-properties (point-min) (point-max))
               text))))


(defun hy--assert-faces (text)
  "Assert text props of TEXT according to `faceup' markup."
  (should (hy--font-lock-test text)))


(defun hy--assert-current-form-string (form-string)
  "Assert FORM-STRING is extracted by `hy--current-form-string'."
  (hy-with-hy-mode
   (insert form-string)
   (forward-char -1)
   (s-assert (s-concat form-string "\n")
             (hy--current-form-string))))

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


;; FAIL No argument same line case - Expected
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

;;; Font Lock Tests
;;;; Definitions

(ert-deftest font-lock::builtins ()
  :tags '(font-lock display)
  (hy--assert-faces "«b:+» «b:setv» «b:ap-each» setv-foo foo-setv setv.foo"))


(ert-deftest font-lock::constants ()
  :tags '(font-lock display)
  (hy--assert-faces "«c:True» Truex xTrue"))


(ert-deftest font-lock::defs ()
  :tags '(font-lock display)
  (hy--assert-faces "«k:defn» «f:func» defnx func xdefn func"))


(ert-deftest font-lock::exceptions ()
  :tags '(font-lock display)
  (hy--assert-faces "«t:ValueError» ValueErrorx xValueError"))


(ert-deftest font-lock::special-forms ()
  :tags '(font-lock display)
  (hy--assert-faces "«k:->» «k:for» forx xfor for.x x.for"))

;;;; Static

(ert-deftest font-lock::aliases ()
  :tags '(font-lock display)
  (hy--assert-faces "«k:defmacro-alias» [«f:arg1 arg2 arg3»]"))


(ert-deftest font-lock::classes ()
  :tags '(font-lock display)
  (hy--assert-faces "«k:defclass» «t:Klass» [parent]"))


(ert-deftest font-lock::decorators-tag ()
  :tags '(font-lock display)
  (hy--assert-faces "«t:#@(a-dec» func-def)"))


(ert-deftest font-lock::decorators-with ()
  :tags '(font-lock display)
  (hy--assert-faces "(«t:with-decorator a-dec» func-def)"))


(ert-deftest font-lock::imports ()
  :tags '(font-lock display)
  (hy--assert-faces "(«k:import» x [y «k::as» z])")
  (hy--assert-faces "(«k:require» x [y «k::as» z])"))


(ert-deftest font-lock::self ()
  :tags '(font-lock display)
  (hy--assert-faces "«k:self» x «k:self».x self-x x-self"))


(ert-deftest font-lock::tag-macros ()
  :tags '(font-lock display)
  (hy--assert-faces "«f:#tag-x» y"))


(ert-deftest font-lock::variables ()
  :tags '(font-lock display)
  ;; Someday it would be nice to work with unpacking/multiple clauses
  (hy--assert-faces "(«b:setv» «v:foo» bar)"))

;;;; Misc

(ert-deftest font-lock::anonymous-funcs ()
  :tags '(font-lock display)
  (-let [hy-font-lock-highlight-percent-args? t]
    (hy--assert-faces "«v:%1» «v:%12» «v:%1».foo %foo %1foo «b:%»")))


(ert-deftest font-lock::func-kwargs ()
  :tags '(font-lock display)
  (hy--assert-faces "«t:&rest» args «t:&kwargs» kwargs"))


(ert-deftest font-lock::kwargs ()
  :tags '(font-lock display)
  (hy--assert-faces "«c::kwarg-1» foo «c::kwarg-2»"))


(ert-deftest font-lock::shebang ()
  :tags '(font-lock display)
  (hy--assert-faces "«x:#!shebang»\ncode"))


(ert-deftest font-lock::unpacking-generalizations ()
  :tags '(font-lock display)
  (hy--assert-faces "«k:#*» args «k:#**» kwargs"))

;;; Syntax Tests
;;;; Symbols

(ert-deftest syntax::symbols-include-dots ()
  :tags '(syntax)
  (hy-with-hy-mode
   (insert "foo.bar")
   (s-assert "foo.bar" (thing-at-point 'symbol))))


(ert-deftest syntax::symbols-include-dashes ()
  :tags '(syntax)
  (hy-with-hy-mode
   (insert "foo-bar")
   (s-assert "foo-bar" (thing-at-point 'symbol))))


(ert-deftest syntax::symbols-include-tags ()
  :tags '(syntax)
  (hy-with-hy-mode
   (insert "#foo")
   (s-assert "#foo" (thing-at-point 'symbol))))


(ert-deftest syntax::symbols-exclude-quote-chars ()
  :tags '(syntax)
  (hy-with-hy-mode
   (insert "'foo")
   (s-assert "foo" (thing-at-point 'symbol))
   (insert "`foo")
   (s-assert "foo" (thing-at-point 'symbol))
   (insert "~foo")
   (s-assert "foo" (thing-at-point 'symbol))
   (insert "~@foo")
   (s-assert "foo" (thing-at-point 'symbol))))

;;;; Comments

(ert-deftest syntax::comments-single-char ()
  :tags '(syntax)
  (hy--assert-faces "foo «x:; bar»"))


(ert-deftest syntax::comments-two-char ()
  :tags '(syntax)
  (hy--assert-faces "foo «m:;; »«x:bar»"))


(ert-deftest syntax::comments-many-char ()
  :tags '(syntax)
  (hy--assert-faces "foo «m:;;;;;;; »«x:bar»"))

;;;; Context Sensitive

(ert-deftest syntax::bracket-strings-string-fence-set-one-line ()
  :tags '(syntax)
  (hy--assert-faces "#[«s:[foo]»]"))


(ert-deftest syntax::bracket-strings-string-fence-set-many-lines ()
  :tags '(syntax)
  (hy--assert-faces "#[«s:[foo\n\nbar]»]"))


(ert-deftest syntax::bracket-strings-string-fence-set-with-quote-chars ()
  :tags '(syntax)
  (hy--assert-faces "#[«s:[\"foo\"]»]"))


(ert-deftest syntax::bracket-strings-string-fence-set-with-matching-delims ()
  :tags '(syntax)
  (hy--assert-faces "#[delim«s:[foo]»delim]"))


(ert-deftest syntax::bracket-strings-string-fence-set-with-different-delims ()
  :tags '(syntax)
  (hy--assert-faces "#[delim-1«s:[foo]»delim-2]"))


(ert-deftest syntax::bracket-strings-many-bracket-strings ()
  :tags '(syntax)
  (hy--assert-faces "#[«s:[foo]»] \nfoo \n #[«s:[foo]»]"))


(ert-deftest syntax::bracket-strings-indentation ()
  :tags '(syntax indentation)
  (hy--assert-indented "
(#[[a
b]])

(#[[a

b]])

(#[-[a
b]-])

(#[-+[a
b]+-])

(#[no second bracket
   => not a string])
"))

;;; Docstring Detection Tests

(ert-deftest docstrings::module-docstrings ()
  :tags '(font-lock)
  (hy-with-hy-mode
   (hy--assert-faces "«d:\"foo\"»")
   (hy--assert-faces " «s:\"foo\"»")))


(ert-deftest docstrings::func-docstrings-use-parent-form-correctly ()
  :tags '(font-lock)
  (hy-with-hy-mode
   (hy--assert-faces "(«k:defn» «f:foo» [] «d:\"bar\"»)")
   (hy--assert-faces "(«k:defn» «f:foo» [] [«s:\"bar\"»])")))


;; FAIL Known minor issue
;; (ert-deftest docstrings::func-docstrings-only-first-string-a-docstring ()
;;   :tags '(font-lock)
;;   (hy-with-hy-mode
;;    (hy--assert-faces "(«k:defn» «f:foo» [] «d:\"bar\"» «s:\"baz\"»)")))

;;; Keybindings

(ert-deftest keybinding::insert-pdbs ()
  :tags '(misc)
  (hy-with-hy-mode
   (hy-insert-pdb)
   (s-assert (buffer-string)
             "(do (import pdb) (pdb.set-trace))")
   (delete-region (point-min) (point-max))
   (hy-insert-pdb-threaded)
   (s-assert (buffer-string)
             "((fn [x] (import pdb) (pdb.set-trace) x))")))

;; `hy-shell-eval-buffer'
;; `hy-shell-eval-region'
;; `hy-shell-eval-current-form'
;; `hy-shell-start-or-switch-to-shell'

;;; Misc Tests

(ert-deftest misc::current-form-string-extracts-bracket-likes ()
  :tags '(misc)
  (hy--assert-current-form-string "[foo]")
  (hy--assert-current-form-string "{foo bar}"))


(ert-deftest misc::current-form-string-extracts-simple-form ()
  :tags '(misc)
  (hy--assert-current-form-string "(foo)")
  (hy--assert-current-form-string "(foo bar)"))


(ert-deftest misc::current-form-string-extracts-form-with-forms ()
  :tags '(misc)
  (hy--assert-current-form-string "(foo (bar))"))

;;; Shell
;;;; No Process Requirement

(ert-deftest shell::process-names ()
  :tags '(shell)
  (s-assert (hy--shell-format-process-name "Foo")
            "*Foo*"))


(ert-deftest shell::interpreter-args-no-args ()
  :tags '(shell)
  (let ((hy-shell-interpreter-args "")
        (hy-shell-use-control-codes? nil))
    (should (s-blank? (hy--shell-calculate-interpreter-args))))

  (let ((hy-shell-interpreter-args "")
        (hy-shell-use-control-codes? t))
    (should (s-blank? (hy--shell-calculate-interpreter-args)))))


(ert-deftest shell::interpreter-args-args-no-spy ()
  :tags '(shell)
  (let ((hy-shell-interpreter-args "foo")
        (hy-shell-use-control-codes? nil))
    (s-assert (hy--shell-calculate-interpreter-args)
              "foo"))

  (let ((hy-shell-interpreter-args "foo")
        (hy-shell-use-control-codes? t))
    (s-assert (hy--shell-calculate-interpreter-args)
              "foo")))


(ert-deftest shell::interpreter-args-args-with-spy ()
  :tags '(shell)
  (let ((hy-shell-interpreter-args "--spy")
        (hy-shell-use-control-codes? nil))
    (s-assert (hy--shell-calculate-interpreter-args)
              "--spy"))

  (let ((hy-shell-interpreter-args "--spy")
        (hy-shell-use-control-codes? t))
    (s-assert (hy--shell-calculate-interpreter-args)
              "--spy --control-codes")))

;;;; Requires Process

;; (ert-deftest shell::get-process ()
;;   :tags '(shell)
;;   (skip-unless (hy-installed?))

;;   (let ((hy-shell-buffer-name "foo")
;;         (hy-shell-internal-buffer-name "bar"))
;;     (should (s-equals? (hy-shell-get-process)
;;                        "foo"))
;;     (should (s-equals? (hy-shell-get-process 'internal)
;;                        "bar"))))



;; (defun hy--ert-start-shell-internal ()
;;   ;; (hy--shell-kill-buffer)
;;   (hy--shell-make-comint (hy--shell-calculate-command)
;;                         hy-shell-internal-buffer-name 'internal))

(defmacro hy-with-hy-shell (&rest forms)
  `(-let [hy-shell-interpreter-args ""]
     (hy--shell-make-comint (hy--shell-calculate-command) hy-shell-buffer-name)
     (set-process-query-on-exit-flag (hy-shell-get-process) nil)
     ,@forms
     (hy--shell-kill-buffer)))


(ert-deftest shell::comint-manages-hy-shell-buffer-var ()
  :tags '(shell)
  (should-not (hy--shell-buffer?))
  (hy-with-hy-shell
   (should (hy--shell-buffer?)))
  (should-not (hy--shell-buffer?)))
