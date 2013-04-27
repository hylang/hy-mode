;;; hy-mode.el --- Major mode for Hy code

;; Copyright © 2013 Julien Danjou <julien@danjou.info>
;;
;; Authors: Julien Danjou <julien@danjou.info>
;; URL: http://github.com/hylang/hy-mode
;; Version: 1.0
;; Keywords: languages, lisp

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
              (regexp-opt '("defn" "defclass" "import"))
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
                 "except" "if" "assert" "global" "lambda" "fn" "yield"
                 "decorate-with" "with" "," "list-comp" "kwapply"
                 "while" "let"))
              "\\)\\>")
     (1 font-lock-keyword-face))))


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
      (let ((open-paren (elt state 1))
            (function (buffer-substring (point)
                                        (progn (forward-sexp 1) (point)))))
        (cond ((member (char-after open-paren) '(?\[ ?\{))
               (goto-char open-paren)
               (1+ (current-column)))
              ((string-match-p "\\`\\(?:\\S +/\\)?\\(def\\|with-\\)" function)
               (lisp-indent-defform state indent-point)))))))

;;;###autoload
(progn
  (add-to-list 'auto-mode-alist '("\\.hy\\'" . hy-mode))
  (add-to-list 'interpreter-mode-alist '("hy" . hy-mode)))

;;;###autoload
(define-derived-mode hy-mode prog-mode "Hy"
  "Major mode for editing Hy files."
  (setq font-lock-defaults
        '(hy-font-lock-keywords))
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
