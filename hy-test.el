;;; hy-test.el --- Testing Macros -*- lexical-binding: t -*-

(require 'buttercup)
(require 'faceup)
(require 'hy-mode)

;;; Buttercup Extensions
;;;; Macros

(defalias 'xnt-describe #'xdescribe)

(defmacro nt-describe (description &rest body)
  "Equivalent to buttercup's `describe' but uses `-let*' on `:var' bindings."
  (declare (indent 1) (debug (&define sexp def-body)))
  (let ((new-body (if (eq (elt body 0) :var)
                      `((-let* ,(elt body 1)
                          ,@(cddr body)))
                    body)))
    `(buttercup-describe ,description (lambda () ,@new-body))))

;;;; Matchers
;;;;; General Purpose

(buttercup-define-matcher-for-unary-function :nil null)

(buttercup-define-matcher :size (obj size)
  (let ((obj (funcall obj))
        (size (funcall size)))
    (if (= (length obj) size)
        t
      `(nil . ,(format "Expected %s of size %s to have size %s"
                     obj (length obj) size)))))

;;;;; Indentation

(defun hy-inhibit-progress-reporter (&rest args))

(buttercup-define-matcher :indented (text)
  (let* ((text
          (s-trim (funcall text)))
         (text-no-indent
          (->> text s-lines (-map 's-trim-left) (s-join "\n"))))
    (insert text-no-indent)

    (advice-add #'make-progress-reporter :override #'hy-inhibit-progress-reporter)
    (indent-region-line-by-line (point-min) (point-max))
    (advice-remove #'make-progress-reporter #'hy-inhibit-progress-reporter)

    (let ((indented-text
           (s-trim (buffer-substring-no-properties (point-min) (point-max)))))
      (delete-region (point-min) (point-max))

      (if (string= text indented-text)
          t
        `(nil . ,(format "Given indented text \n%s\n was instead indented to \n%s\n"
                       text indented-text))))))

;;;;; Syntax Highlighting

(buttercup-define-matcher :faces (text)
  (let ((text (funcall text))
        (hy-shell-internal?))
    (when (fboundp 'rainbow-delimiters-mode-disable)
      (advice-add 'hy-mode :after 'rainbow-delimiters-mode-disable))
    (if (prog1 (faceup-test-font-lock-string 'hy-mode text)
          (advice-remove 'hy-mode 'rainbow-delimiters-mode-disable))
        t
      `(nil . ,(format "Faceup for %s failed" text)))))

;;; Hy Testing Contexts

;; (defun hy-test--setup (&optional text)
;;   (hy-mode)
;;   (when text
;;     (insert (s-trim text))))

;; (defun hy-test--teardown ()
;;   "Disable notate and clear the buffer."
;;   (delete-region (point-min) (point-max)))

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


(defun hy--font-lock-test (text)
  "Entry to test faces of TEXT markedup as faceup, disabling minor modes faces.

See `faceup-face-short-alist' for faceup's face aliases."
  (when (fboundp 'rainbow-delimiters-mode-disable)
    (advice-add 'hy-mode :after 'rainbow-delimiters-mode-disable))
  (prog1
      (faceup-test-font-lock-string 'hy-mode text)
    (advice-remove 'hy-mode 'rainbow-delimiters-mode-disable)))
(faceup-defexplainer hy--font-lock-test)
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

;;; Provide

(provide 'hy-test)
