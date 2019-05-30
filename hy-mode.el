;;; hy-mode.el --- Major mode for Hylang -*- lexical-binding: t -*-

;; Copyright © 2013-2016 Julien Danjou <julien@danjou.info>
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

;; Provides font-lock, indentation, navigation, autocompletion, and other
;; features for working productively in Hy (http://hylang.org).

;; Hy is a lisp embedded in Python.

;;; Code:

(require 'hy-base)

;; TODO Add a hy-syntax and hy-jedhy pkgs
;; TODO Add a hy-config file?
(require 'hy-font-lock)
(require 'hy-shell)

(defgroup hy-mode nil
  "A mode for Hy"
  :prefix "hy-mode-"
  :group 'applications)

;;; Configuration
;;;; Indentation

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
    (modify-syntax-entry ?\{ "(}" table)
    (modify-syntax-entry ?\} "){" table)
    (modify-syntax-entry ?\[ "(]" table)
    (modify-syntax-entry ?\] ")[" table)

    ;; Quote characters are prefixes
    (modify-syntax-entry ?\~ "'" table)
    (modify-syntax-entry ?\@ "'" table)

    ;; "," is a symbol in Hy, namely the tuple constructor
    (modify-syntax-entry ?\, "_" table)

    ;; "|" is a symbol in hy, naming the or operator
    (modify-syntax-entry ?\| "_" table)

    ;; "#" is a tag macro, we include # in the symbol
    (modify-syntax-entry ?\# "_" table)

    table)
  "Hy mode's syntax table.")

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

(defun hy-indent--syntax->spec (syntax)
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

        ((hy-indent--syntax->spec syntax)
         (1+ (current-column)))

        (t (hy-indent--normal calculate-lisp-indent-last-sexp))))

;;; Describe thing at point

;; (defun hy--docs-for-thing-at-point ()
;;   "Mirrors `hy-eldoc-documentation-function' formatted for a buffer, not a msg."
;;   (-> (thing-at-point 'symbol)
;;      (hy--eldoc-get-docs t)
;;      hy--format-docs-for-buffer))

;; (defun hy--format-docs-for-buffer (text)
;;   "Format raw hydoc TEXT for inserting into hyconda buffer."
;;   (-let [kwarg-newline-regexp
;;          (rx ","
;;              (1+ (not (any "," ")")))
;;              (group-n 1 "\\\n")
;;              (1+ (not (any "," ")"))))]
;;     (-some--> text
;;             (s-replace "\\n" "\n" it)
;;             (replace-regexp-in-string kwarg-newline-regexp
;;                                       "newline" it nil t 1))))

;; (defun hy-describe-thing-at-point ()
;;   "Implement shift-k docs lookup for `spacemacs/evil-smart-doc-lookup'."
;;   (interactive)
;;   (-when-let* ((text (hy--docs-for-thing-at-point))
;;                (doc-buffer "*Hyconda*"))
;;     (with-current-buffer (get-buffer-create doc-buffer)
;;       (erase-buffer)
;;       (switch-to-buffer-other-window doc-buffer)

;;       (insert text)
;;       (goto-char (point-min))
;;       (forward-line)

;;       (insert "------\n")
;;       (fill-region (point) (point-max))

;;       ;; Eventually make hyconda-view-minor-mode, atm this is sufficient
;;       (local-set-key "q" 'quit-window)
;;       (when (fboundp 'evil-local-set-key)
;;         (evil-local-set-key 'normal "q" 'quit-window)))))

;;; Keybindings

;;;###autoload
(defun hy-insert-pdb ()
  "Import and set pdb trace at point."
  (interactive)
  (insert "(do (import pdb) (pdb.set-trace))"))

;;; hy-mode and inferior-hy-mode
;;;; Hy-mode setup

(defun hy--mode-setup-eldoc ()
  (make-local-variable 'eldoc-documentation-function)
  (setq-local eldoc-documentation-function 'hy-eldoc-documentation-function)
  (eldoc-mode +1))

(defun hy--mode-setup-font-lock ()
  (setq-local font-lock-multiline t)
  (setq font-lock-defaults
        '(hy-font-lock-kwds
          nil nil
          (("+-*/.<>=!?$%_&~^:@" . "w"))  ; syntax alist
          nil
          (font-lock-mark-block-function . mark-defun)
          (font-lock-syntactic-face-function  ; Differentiates (doc)strings
           . hy-font-lock-syntactic-face-function))))

(defun hy--mode-setup-inferior ()
  ;; (add-to-list 'company-backends 'company-hy)
  (setenv "PYTHONIOENCODING" "UTF-8")

  (run-hy-internal)
  (add-hook 'pyvenv-post-activate-hooks 'run-hy-internal nil t))

(defun hy--mode-setup-syntax ()
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
  (setq-local indent-line-function 'lisp-indent-line)
  (setq-local lisp-indent-function 'hy-indent-function))

(defun hy--mode-setup-smartparens ()
  "Setup smartparens, if active, pairs for Hy."
  (when (fboundp 'sp-local-pair)
    (sp-local-pair '(hy-mode) "`" "`" :actions nil)
    (sp-local-pair '(hy-mode) "'" "'" :actions nil)
    (sp-local-pair '(inferior-hy-mode) "`" "" :actions nil)
    (sp-local-pair '(inferior-hy-mode) "'" "" :actions nil)))

;;; Core

(add-to-list 'auto-mode-alist '("\\.hy\\'" . hy-mode))
(add-to-list 'interpreter-mode-alist '("hy" . hy-mode))

;;;###autoload
(define-derived-mode hy-mode prog-mode "Hy"
  "Major mode for editing Hy files."
  (hy--mode-setup-font-lock)
  (hy--mode-setup-eldoc)
  (hy--mode-setup-smartparens)
  (hy--mode-setup-syntax)

  ;; (run-hy-internal)
  ;; (add-hook 'pyvenv-post-activate-hooks 'run-hy-internal nil t)
  )

;; Spacemacs users please see spacemacs-hy, all bindings defined there

(set-keymap-parent hy-mode-map lisp-mode-shared-map)
(define-key hy-mode-map (kbd "C-c C-z") #'run-hy)

(define-key hy-mode-map (kbd "C-c C-b") 'hy-shell-eval-buffer)
(define-key hy-mode-map (kbd "C-c C-r") 'hy-shell-eval-region)
(define-key hy-mode-map (kbd "C-M-x") 'hy-shell-eval-current-form)
(define-key hy-mode-map (kbd "C-c C-e") 'hy-shell-eval-last-sexp)

(define-key hy-mode-map (kbd "C-c C-t") #'hy-insert-pdb)

(define-key inferior-hy-mode-map (kbd "C-c C-z") (lambda ()
                                                   (interactive)
                                                   (other-window -1)))

(provide 'hy-mode)

;;; hy-mode.el ends here
