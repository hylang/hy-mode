;;; hy-mode.el --- Major mode for Hy code

;; Copyright © 2013 Julien Danjou <julien@danjou.info>
;;
;; Authors: Julien Danjou <julien@danjou.info>
;; URL: http://github.com/hylang/hy-mode
;; Version: 1.0
;; Keywords: languages, lisp

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

;; Provides font-lock, indentation, and navigation for the Hy
;; language. (http://hylang.org)

(defgroup hy-mode nil
  "A mode for Hy"
  :prefix "hy-mode-"
  :group 'applications)

(defcustom hy-mode-inferior-lisp-command "hy"
  "The command used by `inferior-lisp-program'."
  :type 'string
  :group 'hy-mode)

(defconst hy-font-lock-keywords
  `((,(concat "(\\("
              (regexp-opt '("defn" "defun" "defclass" "import"
                            "defmacro" "require"))
              "\\)\\>"
              ;; Spaces
              "[ \r\n\t]+"
              ;; Function/class name
              "\\([^ \r\n\t()]+\\)")
     (1 font-lock-keyword-face)
     (2 font-lock-function-name-face nil t))
    (,(concat "(\\("
              (regexp-opt
               '("do" "for" "foreach" "try" "throw" "raise" "progn" "catch"
                 "else" "finally" "except" "if" "unless" "when" "assert" "global"
                 "lambda" "fn" "yield" "with-decorator" "with_decorator" "with"
                 "kwapply" "while" "let" "cond" "_>" "->" "_>>" "->>"))
              "\\)[ \n\r\t)]")
     (1 font-lock-keyword-face))
    (,(concat "(\\("
              (regexp-opt
               '("get" "slice" "assoc" "," "quote" "eval" "list-comp" "not" "~"
                 "and" "or" "=" "!=" "<" "<=" ">" ">=" "is" "in" "is-not"
                 "is_not" "not-in" "not_in" "+" "%" "/" "//" "*" "**" "<<" ">>"
                 "|" "^" "&" "-" "+=" "/=" "//=" "*=" "-=" "_=" "%=" "**=" "<<="
                 ">>=" "|=" "^=" "&=" "setv" "def" "car" "first" "cdr" "rest"
                 "take" "drop" "print" "quasiquote" "unquote" "unquote-splice"))
              "\\)[ \n\r\t)]")
     (1 font-lock-builtin-face))
    ("\\<:[^ \r\n\t]+\\>" 0 font-lock-constant-face)))

(defcustom hy-indent-specform
  '(("for" . 1)
    ("foreach" . 1)
    ("while" . 1)
    ("except" . 1)
    ("catch" . 1)
    ("let" . 1)
    ("if" . 1)
    ("when" . 1)
    ("unless" . 1))
  "How to indent specials specform."
  :group 'hy-mode)

(defun hy-indent-function (indent-point state)
  "This function is the normal value of the variable `lisp-indent-function' for `hy-mode'.
It is used when indenting a line within a function call, to see
if the called function says anything special about how to indent
the line.

INDENT-POINT is the position at which the line being indented begins.
Point is located at the point to indent under (for default indentation);
STATE is the `parse-partial-sexp' state for that position.

This function returns either the indentation to use, or nil if the
Lisp function does not specify a special indentation."
  (let ((normal-indent (current-column)))
    (goto-char (1+ (elt state 1)))
    (parse-partial-sexp (point) calculate-lisp-indent-last-sexp 0 t)
    (if (and (elt state 2)
             (not (looking-at "\\sw\\|\\s_")))
        ;; car of form doesn't seem to be a symbol
        (progn
          (if (not (> (save-excursion (forward-line 1) (point))
                      calculate-lisp-indent-last-sexp))
              (progn (goto-char calculate-lisp-indent-last-sexp)
                     (beginning-of-line)
                     (parse-partial-sexp (point)
                                         calculate-lisp-indent-last-sexp 0 t)))
          ;; Indent under the list or under the first sexp on the same
          ;; line as calculate-lisp-indent-last-sexp.  Note that first
          ;; thing on that line has to be complete sexp since we are
          ;; inside the innermost containing sexp.
          (backward-prefix-chars))
      (let* ((open-paren (elt state 1))
             (function (buffer-substring (point)
                                         (progn (forward-sexp 1) (point))))
             (specform (cdr (assoc function hy-indent-specform))))
        (cond ((member (char-after open-paren) '(?\[ ?\{))
               (goto-char open-paren)
               (1+ (current-column)))
              (specform
               (lisp-indent-specform specform state indent-point normal-indent))
              ((string-match-p "\\`\\(?:\\S +/\\)?\\(def\\|with-\\|with_\\|fn\\|lambda\\)" function)
               (lisp-indent-defform state indent-point)))))))

;;;###autoload
(progn
  (add-to-list 'auto-mode-alist '("\\.hy\\'" . hy-mode))
  (add-to-list 'interpreter-mode-alist '("hy" . hy-mode)))

(defvar hy-mode-syntax-table
  (let ((table (copy-syntax-table lisp-mode-syntax-table)))
    (modify-syntax-entry ?\{ "(}" table)
    (modify-syntax-entry ?\} "){" table)
    (modify-syntax-entry ?\[ "(]" table)
    (modify-syntax-entry ?\] ")[" table)
    table))

(unless (fboundp 'setq-local)
  (defmacro setq-local (var val)
    `(set (make-local-variable ',var) ,val)))

;;;###autoload
(define-derived-mode hy-mode prog-mode "Hy"
  "Major mode for editing Hy files."
  (setq font-lock-defaults
        '(hy-font-lock-keywords
          nil nil
          (("+-*/.<>=!?$%_&~^:@" . "w")) ; syntax alist
          nil
          (font-lock-mark-block-function . mark-defun)
          (font-lock-syntactic-face-function
           . lisp-font-lock-syntactic-face-function)))
  (setq-local comment-start ";")
  (setq-local comment-start-skip
              "\\(\\(^\\|[^\\\\\n]\\)\\(\\\\\\\\\\)*\\)\\(;+\\|#|\\) *")
  (setq-local comment-add 1)
  (setq-local inferior-lisp-program hy-mode-inferior-lisp-command)
  (setq-local indent-line-function 'lisp-indent-line)
  (setq-local lisp-indent-function 'hy-indent-function))

(set-keymap-parent hy-mode-map lisp-mode-shared-map)
(define-key hy-mode-map (kbd "C-M-x")   'lisp-eval-defun)
(define-key hy-mode-map (kbd "C-x C-e") 'lisp-eval-last-sexp)
(define-key hy-mode-map (kbd "C-c C-z") 'switch-to-lisp)

(provide 'hy-mode)

;;; hy-mode.el ends here
