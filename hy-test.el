;;; hy-test.el --- Testing Macros -*- lexical-binding: t -*-

;; Copyright © 2013 Julien Danjou <julien@danjou.info>
;;           © 2017-2019 Eric Kaschalk <ekaschalk@gmail.com>
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

;; Macros and others to support buttercup-based tests.

;;; Code:

(require 'buttercup)
(require 'faceup)
(require 'pyvenv)

(require 'hy-mode)

;;; Configuration

(defvar hy-test--pyvenv-name "hackingenv"
  "The name of a python virtual environment with Hy installed.

If no name is given, then process-based tests will be skipped.")

;;; Buttercup Extensions

(defalias 'xnt-describe #'xdescribe)
(defalias 'xnt-shell-describe #'xdescribe)

(defmacro nt-describe (description &rest body)
  "Equivalent to buttercup's `describe' but uses `-let*' on `:var' bindings."
  (declare (indent 1) (debug (&define sexp def-body)))
  (let ((new-body (if (eq (elt body 0) :var)
                      `((-let* ,(elt body 1)
                          ,@(cddr body)))
                    body)))
    `(buttercup-describe ,description (lambda () ,@new-body))))

(defmacro nt-shell-describe (description &rest body)
  "Run `nt-describe' with messages if hy interpreter not available."
  (declare (indent 1) (debug (&define sexp def-body)))
  (if (hy-shell--check-installed?)
      `(nt-describe ,description ,@body)
    `(describe ,description
       (describe "Tests are skipped - hy not found, see hy-test.el."))))

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
  (let ((text (funcall text)))
    (when (fboundp 'rainbow-delimiters-mode-disable)
      (advice-add 'hy-mode :after 'rainbow-delimiters-mode-disable))

    ;; Later add the result to the fail message by doing
    ;; the fontifying manually, like I do in :shell-faces matcher
    (prog1 (if (faceup-test-font-lock-string 'hy-mode text)
               t
             `(nil . ,(format "Faceup for %s failed" text)))
      (advice-remove 'hy-mode 'rainbow-delimiters-mode-disable))))

;; Tests `inferior-hy-font-lock-kwds'. These keywords are relatively advanced
;; as they do additional checks based on comint variables.
(buttercup-define-matcher :shell-faces (text)
  (let ((text (funcall text))
        (prompt-length (length "=> ")))
    ;; Build the Hy shell buffer and manually enable font locking
    (hy-test--run-hy)
    (hy-inferior--support-font-locking-input)
    (insert text)

    ;; Faceup and fontify
    (faceup-clean-buffer)
    (font-lock-fontify-region (point-min) (point-max))

    ;; Delete *after* fontifying, the keywords reference `comint-last-prompt'!
    (with-silent-modifications
      (delete-region (point-min) (+ prompt-length (car comint-last-prompt))))

    ;; Compare and return result
    (let ((result (faceup-markup-buffer)))
      (prog1 (if (faceup-test-equal text result)
                 t
               `(nil . ,(format "Faceup for %s failed instead was %s"
                              text result)))
        (hy-shell--kill)))))

;;; Process Tests

(defun hy-test--setup-env ()
  "Setup virtual env for process-based tests."
  (if hy-test--pyvenv-name
      (if (ignore-errors (prog1 t (pyvenv-workon hy-test--pyvenv-name)))
          (message "Pyvenv started successfully for process-based tests.")
        (message "Pyvenv failed to start for tests!"))
    (message "`hy-test--pyvenv-name' is not set - no Hy process tests ran!")))

(defun hy-test--run-hy ()
  "Do `run-hy' with some extra test friendly settings."
  (let ((hy-shell--notify?))
    (run-hy)
    (switch-to-buffer hy-shell--buffer-name)
    (set-process-query-on-exit-flag (hy-shell--current-process) nil)))

;;; Provide

(provide 'hy-test)

;;; hy-test.el ends here
