;;; hy-test.el --- Testing Macros -*- lexical-binding: t -*-

(require 'buttercup)
(require 'faceup)
(require 'hy-mode)

;;; Buttercup Extensions

(defalias 'xnt-describe #'xdescribe)

(defmacro nt-describe (description &rest body)
  "Equivalent to buttercup's `describe' but uses `-let*' on `:var' bindings."
  (declare (indent 1) (debug (&define sexp def-body)))
  (let ((new-body (if (eq (elt body 0) :var)
                      `((-let* ,(elt body 1)
                          ,@(cddr body)))
                    body)))
    `(buttercup-describe ,description (lambda () ,@new-body))))

;;; Buttercup Matchers
;;;; General Purpose

(buttercup-define-matcher-for-unary-function :nil null)

(buttercup-define-matcher :size (obj size)
  (let ((obj (funcall obj))
        (size (funcall size)))
    (if (= (length obj) size)
        t
      `(nil . ,(format "Expected %s of size %s to have size %s"
                     obj (length obj) size)))))

;;;; Indentation

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

;;;; Syntax Highlighting

(buttercup-define-matcher :faces (text)
  (let ((text (funcall text))
        (hy-shell-internal?))
    (when (fboundp 'rainbow-delimiters-mode-disable)
      (advice-add 'hy-mode :after 'rainbow-delimiters-mode-disable))
    (if (prog1 (faceup-test-font-lock-string 'hy-mode text)
          (advice-remove 'hy-mode 'rainbow-delimiters-mode-disable))
        t
      `(nil . ,(format "Faceup for %s failed" text)))))

;;; Provide

(provide 'hy-test)
