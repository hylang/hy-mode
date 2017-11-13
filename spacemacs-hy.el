(provide 'spacemacs-hy)

;; Disable this unless using special branch
(setq hy-shell-use-control-codes? nil)

;; Autocompletion now fit for use, not all symbols complete, hy bug
(spacemacs|add-company-backends
  :backends company-hy
  :modes hy-mode inferior-hy-mode)

(spacemacs/declare-prefix-for-mode 'hy-mode "me" "eval")
(spacemacs/declare-prefix-for-mode 'hy-mode "md" "debug")
(spacemacs/declare-prefix-for-mode 'hy-mode "mt" "test")
(spacemacs/declare-prefix-for-mode 'hy-mode "mV" "pyvenv")

(spacemacs/set-leader-keys-for-major-mode 'hy-mode
  "," 'lisp-state-toggle-lisp-state

  "'" 'hy-shell-start-or-switch-to-shell
  "ei" 'hy-shell-start-or-switch-to-shell
  "eb" 'hy-shell-eval-buffer
  "er" 'hy-shell-eval-region
  "ec" 'hy-shell-eval-current-form

  "dd" 'hy-insert-pdb
  "dt" 'hy-insert-pdb-threaded

  "tA" 'spacemacs/python-test-pdb-all
  "ta" 'spacemacs/python-test-all
  "tM" 'spacemacs/python-test-pdb-module
  "tm" 'spacemacs/python-test-module

  "hh" 'hy-describe-thing-at-point)  ; Otherwise known as shift-K

(spacemacs/set-leader-keys-for-major-mode 'inferior-hy-mode
  "," 'lisp-state-toggle-lisp-state)
