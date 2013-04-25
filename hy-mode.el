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
  (setq-local inferior-lisp-program hy-mode-inferior-lisp-command))

(set-keymap-parent hy-mode-map lisp-mode-shared-map)
(define-key hy-mode-map (kbd "C-M-x")   'lisp-eval-defun)
(define-key hy-mode-map (kbd "C-x C-e") 'lisp-eval-last-sexp)
(define-key hy-mode-map (kbd "C-c C-z") 'switch-to-lisp)

(provide 'hy-mode)

;;; hy-mode.el ends here
