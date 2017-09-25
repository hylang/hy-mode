(provide 'spacemacs-hy)

(setq hy-shell-use-control-codes? t)

(spacemacs/set-leader-keys-for-major-mode 'hy-mode
  "ei" 'hy-shell-start-or-switch-to-shell
  "eb" 'hy-eval-buffer)

(spacemacs/declare-prefix-for-mode 'hy-mode "md" "debug")
(spacemacs/set-leader-keys-for-major-mode 'hy-mode
  "dd" 'hy-insert-pdb
  "dt" 'hy-insert-thread-pdb
  "," 'lisp-state-toggle-lisp-state
  "'" 'hy-shell-start-or-switch-to-shell)

(spacemacs/declare-prefix-for-mode 'hy-mode "mt" "test")
(spacemacs/set-leader-keys-for-major-mode 'hy-mode
  "tA" 'spacemacs/python-test-pdb-all
  "ta" 'spacemacs/python-test-all
  "tM" 'spacemacs/python-test-pdb-module
  "tm" 'spacemacs/python-test-module)

(spacemacs/set-leader-keys-for-major-mode 'inferior-hy-mode
  "," 'lisp-state-toggle-lisp-state)
