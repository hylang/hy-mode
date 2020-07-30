;;; hy-mode.el --- Major mode for Hylang -*- lexical-binding: t -*-

;; Copyright © 2013 Julien Danjou <julien@danjou.info>
;;           © 2017-2019 Eric Kaschalk <ekaschalk@gmail.com>
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

;; Provides syntax highlighting, indentation, autocompletion, documentation
;; lookup, REPL support, and more features for working productively in Hy
;; (http://hylang.org), the lisp embedded in Python.

;; Syntax highlighting and related can be found in `hy-font-lock.el'
;; REPL support can be found in `hy-shell.el'
;; IDE components support can be found in `hy-jedhy.el'
;; Common utilities and requires can be found in `hy-base.el'
;; Testing utilities for the test/ folder can be found in `hy-test.el'

;; This file implements the syntax table, indentation, keybindings, and
;; `hy-mode' setup code.

;;; Code:

(require 'hy-base)

(require 'hy-font-lock)
(require 'hy-shell)
(require 'hy-jedhy)

;;; Configuration
;;;; Indentation

;; See other files for configuring specific aspects of Hy, like the shell.

(defvar hy-indent--exactly
  '("when" "unless"
    "for" "for*" "for/a" "for/a*"
    "while"
    "except" "catch")
  "Symbols that will have following lines indented +1 when matched.

Examples:

(when foo
  body)
(when-xx foo
         body)
")


(defvar hy-indent--fuzzily
  '("def"
    "let"
    "with" "with/a"
    "fn" "fn/a")
  "Symbols that will have following lines indented +1 when matched at start.

Examples:

(with foo
  body)
(with-xx foo
  body)
")

;;; Syntax
;;;; Syntax Table

(defconst hy-mode-syntax-table
  (let ((table (copy-syntax-table lisp-mode-syntax-table)))
    ;; List-likes
    (modify-syntax-entry ?\{ "(}" table)
    (modify-syntax-entry ?\} "){" table)
    (modify-syntax-entry ?\[ "(]" table)
    (modify-syntax-entry ?\] ")[" table)

    ;; Quote Characters
    (modify-syntax-entry ?\~ "'" table)

    ;; Symbol Constituents
    (modify-syntax-entry ?\, "_" table)
    (modify-syntax-entry ?\| "_" table)
    (modify-syntax-entry ?\# "_" table)

    ;; Note that @ is a valid symbol token but in almost all usages we would
    ;; rather the symbol for ~@foo to be recognized as foo and not @foo.
    (modify-syntax-entry ?\@ "'" table)

    table)
  "The `hy-mode' syntax table.")

(defconst inferior-hy-mode-syntax-table (copy-syntax-table hy-mode-syntax-table)
  "`inferior-hy-mode' inherits `hy-mode-syntax-table'.")

;;;; Context Sensitive

(defconst hy--bracket-string-rx
  (rx "#["
      (0+ not-newline)
      "["
      (group (1+ (not (any "]"))))
      "]"
      (0+ not-newline)
      "]")
  "Regex identifying Hy's bracket string literals.")

(defun hy-syntax-propertize-function (start end)
  "Implements context sensitive syntax highlighting beyond `font-lock-keywords'.

In particular this implements bracket string literals.
START and END are the limits with which to search for bracket strings passed
and determined by `font-lock-mode' internals when making an edit to a buffer."
  (save-excursion
    (goto-char start)

    ;; Go to the start of the #[[ block
    (when (hy--goto-inner-char (syntax-ppss))
      (ignore-errors (backward-char 2)))

    (while (re-search-forward hy--bracket-string-rx end 'noerror)
      (let ((a (match-beginning 1))
            (b (match-end 1))
            (string-fence (string-to-syntax "|")))
        (put-text-property (1- a) a 'syntax-table string-fence)
        (put-text-property b (1+ b) 'syntax-table string-fence)))))

;;; Indentation
;;;; Normal Indent

(defun hy-indent--normal (calculated-last-sexp-indent)
  "Get indent of the priorly let-bound value `calculate-lisp-indent-last-sexp'

Example:
 (a (b c d
       e
       f))

1. Indent e => start at d (the last sexp) -> c -> b -> err.
=> backwards-sexp will throw error trying to jump to a
=> `hy-indent-function' returns nil
=> black magic then yields the correct indent

2. Indent f => start at e (the last sexp) -> loop done
=> backward-sexp loop terminates because the indentation caught up to the sexp
=> return indent of e

Users interested in the arcane (the nil case) can step through the part of
`calculate-lisp-indent' occurring right after `lisp-indent-function' is called.
Stepping through the trace is particularly useful in understanding indentation
commands."
  (goto-char calculated-last-sexp-indent)

  (let ((last-sexp-start))
    (cond ((ignore-errors
             (while (<= (current-indentation) (current-column))
               (setq last-sexp-start (prog1 (point) (backward-sexp))))
             t)
           (current-column))

          ((null last-sexp-start)
           (progn
             (when (-contains? '(?\' ?\` ?\~ ?\# ?\@) (char-before))
               (backward-char))
             (when (eq ?\~ (char-before))
               (backward-char))

             (1+ (current-column)))))))

;;;; Spec Finding

(defun hy-indent--syntax->indent-spec (syntax)
  "Get int for special indentation for SYNTAX state or nil for normal indent."
  (-when-let (sym (and (hy--prior-sexp? syntax)
                       (thing-at-point 'symbol)))
    (or (-contains? hy-indent--exactly sym)
        (-some (-cut s-matches? <> sym) hy-indent--fuzzily))))

;;;; Indent Function

(defun hy-indent-function (_indent-point syntax)
  "Given SYNTAX, the `parse-partial-sexp' corr. to _INDENT-POINT, get indent."
  (hy--goto-inner-sexp syntax)

  (cond ((-contains? '(?\[ ?\{) (char-before))
         (current-column))

        ((hy-indent--syntax->indent-spec syntax)
         (1+ (current-column)))

        (t (hy-indent--normal calculate-lisp-indent-last-sexp))))

;;; Setup
;;;; Core

(defun hy-mode--setup-font-lock ()
  "Setup `font-lock-defaults' and others for `hy-mode.'"
  (setq-local font-lock-multiline t)
  (setq font-lock-defaults
        '(hy-font-lock-kwds
          nil nil
          (("+-*/.<>=!?$%_&~^:@" . "w"))  ; syntax alist
          nil
          (font-lock-mark-block-function . mark-defun)
          (font-lock-syntactic-face-function  ; Differentiates (doc)strings
           . hy-font-lock-syntactic-face-function))))

(defun hy-mode--setup-syntax ()
  "Setup syntax, indentation, and other core components of major modes."
  ;; We explictly set it for tests that only call this setup-fn
  (set-syntax-table hy-mode-syntax-table)

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
  (setq-local indent-line-function #'lisp-indent-line)
  (setq-local lisp-indent-function #'hy-indent-function))

;;;; Support

(defun hy-mode--support-smartparens ()
  "Setup `smartparens-mode' pairs for Hy, if applicable."
  (when (fboundp #'sp-local-pair)
    (sp-local-pair '(hy-mode inferior-hy-mode) "`" "`" :actions nil)))

;;;; Jedhy

(defun hy-mode--setup-jedhy ()
  "Auto-start jedhy for company, eldoc, and other `hy-mode' IDE features."
  (let ((hy-shell--notify?))
    (run-jedhy))  ; Unlikely that jedhy installed globally so dont warn

  (when (fboundp 'pyvenv-mode)
    (add-hook 'pyvenv-post-activate-hooks #'run-jedhy)
    (add-hook 'pyvenv-post-deactivate-hooks
              #'run-jedhy--pyvenv-post-deactive-hook)))

(defun hy-mode--support-company ()
  "Support `company-mode' autocompletion."
  (add-to-list 'company-backends #'company-hy))

(defun hy-mode--support-eldoc ()
  "Support `eldoc-mode' with lispy docstring leaders."
  (setq-local eldoc-documentation-function #'hy-eldoc-documentation-function)
  (eldoc-mode +1))

;;; hy-mode

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.hy\\'" . hy-mode))
;;;###autoload
(add-to-list 'interpreter-mode-alist '("hy" . hy-mode))

;;;###autoload
(define-derived-mode hy-mode prog-mode "Hy"
  "Major mode for editing Hy files."
  (hy-mode--setup-font-lock)
  (hy-mode--setup-syntax)

  (hy-mode--support-smartparens)

  (when hy-jedhy--enable?
    (hy-mode--setup-jedhy)

    (hy-mode--support-eldoc)

    (when (featurep 'company)
      (hy-mode--support-company)
      (add-hook 'inferior-hy-mode-hook #'hy-mode--support-company))))

;;; Bindings

(set-keymap-parent hy-mode-map lisp-mode-shared-map)

;;;; Shell

(define-key hy-mode-map (kbd "C-c C-z") #'run-hy)

(define-key hy-mode-map (kbd "C-c C-b") #'hy-shell-eval-buffer)
(define-key hy-mode-map (kbd "C-c C-r") #'hy-shell-eval-region)
(define-key hy-mode-map (kbd "C-c C-e") #'hy-shell-eval-last-sexp)
(define-key hy-mode-map (kbd "C-M-x") #'hy-shell-eval-current-form)

(define-key hy-mode-map (kbd "C-c C-d d") #'hy-describe-thing-at-point)
(define-key hy-mode-map (kbd "C-c C-d C-d") #'hy-describe-thing-at-point)

;;;; Misc

;;;###autoload
(defun hy-insert-pdb ()
  "Import and set pdb trace at point."
  (interactive)
  (insert "(do (import pdb) (pdb.set-trace))"))

(define-key hy-mode-map (kbd "C-c C-t") #'hy-insert-pdb)

;;; Provide:

(provide 'hy-mode)

;;; hy-mode.el ends here
