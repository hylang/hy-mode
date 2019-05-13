;; Dumping ground for tests that haven't been ported yet to buttercup atm
;; for one reason or another.

;;; Macros

(defmacro hy-with-hy-mode (&rest forms)
  "Execute FORMS in a temporary `hy-mode' buffer."
  `(with-temp-buffer
     (hy-mode)
     ,@forms))


(defmacro hy-with-hy-shell (&rest forms)
  "Execute FORMS with an active hy process."
  `(-let [hy-shell-interpreter-args ""]
     (hy-shell-kill)
     (save-window-excursion (run-hy))
     (set-process-query-on-exit-flag (hy-shell-get-process) nil)
     ,@forms
     (hy-shell-kill)))


(defmacro hy-with-hy-shell-internal (&rest forms)
  "Execute FORMS with an active hy internal process."
  `(progn
     (hy-shell-kill)
     (run-hy-internal)
     ,@forms
     (hy-shell-kill)))

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

(ert-deftest shell::manages-hy-shell-buffer-vars ()
  :tags '(shell) (skip-unless (hy-installed?))

  (hy-with-hy-shell (should (hy--shell-buffer?)))
  (hy-with-hy-shell-internal (should (hy--shell-buffer? 'internal)))

  (should-not (hy--shell-buffer?))
  (should-not (hy--shell-buffer? 'internal)))


(ert-deftest shell::gets-hy-processes ()
  :tags '(shell) (skip-unless (hy-installed?))

  (hy-with-hy-shell (should (hy-shell-get-process)))
  (hy-with-hy-shell-internal (should (hy-shell-get-process 'internal)))

  (should-not (hy-shell-get-process))
  (should-not (hy-shell-get-process 'internal)))
