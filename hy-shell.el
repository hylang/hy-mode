;;; hy-shell.el --- Shell and Process Support -*- lexical-binding: t -*-

;; Copyright © 2013 Julien Danjou <julien@danjou.info>
;;           © 2017 Eric Kaschalk <ekaschalk@gmail.com>
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

;; Shell and process functionality for Hy.

;;; Code:

(require 'hy-base)

;;; Configuration
;;;; Configured

(defvar hy-shell--interpreter "hy"
  "Default Hy interpreter name.")

(defvar hy-shell--interpreter-args '("--spy")
  "Default argument list to pass to the Hy interpreter.")

(defvar hy-shell--startup-internal-process? t
  "Should an internal process startup for use by ide components?")

(defvar hy-shell--enable-font-lock? t
  "Whether the shell should font-lock the current line.")

(defvar hy-shell--notify? t
  "Allow Hy to message on failure to find Hy, instantiation, shutdown, etc?")

;;;; Managed

(defconst hy-shell--name "Hy"
  "The name to use for the Hy interpreter process.")

(defconst hy-shell--name-internal (format "%s Internal" hy-shell--name)
  "The name to use for the internal Hy interpreter process.")

(defconst hy-shell--buffer-name (s-concat "*" hy-shell--name "*")
  "The buffer name to use for the Hy interpreter process.")

(defconst hy-shell--buffer-name-internal (s-concat "*" hy-shell--name-internal "*")
  "The buffer name to use for the internal Hy interpreter process.")

;;; Macros

(defmacro hy-shell--with (&rest body)
  "Run BODY for Hy process, starting up if needed."
  (declare (indent 0))
  `(when (hy-shell--check-installed?)
     (with-current-buffer (get-buffer-create hy-shell--buffer-name)
       (hy-shell--make-comint)
       ,@body)))

(defmacro hy-shell--with-internal (&rest body)
  "Run BODY for internal Hy process, starting up if needed."
  (declare (indent 0))
  `(when (hy-shell--check-installed?)
     (with-current-buffer (get-buffer-create hy-shell--buffer-name-internal)
       (hy-shell--make-comint-internal)
       ,@body)))

(defmacro hy-shell--with-live (&rest body)
  "Run BODY for Hy process, when it's alive."
  (declare (indent 0))
  `(when (hy-shell--live?)
     (hy-shell--with ,@body)))

(defmacro hy-shell--with-internal-live (&rest body)
  "Run BODY for internal Hy process, when it's alive."
  (declare (indent 0))
  `(when (hy-shell--live-internal?)
     (hy-shell--with-internal ,@body)))

;;; Process Management
;;;; Utilities

(defun hy-shell--live? ()
  "Is the Hy intereprter process alive?"
  (get-buffer hy-shell--buffer-name))

(defun hy-shell--live-internal? ()
  "Is the internal Hy intereprter process alive?"
  (get-buffer hy-shell--buffer-name-internal))

(defun hy-shell--current-process ()
  "Run `get-buffer-process' on the `current-buffer'."
  (get-buffer-process (current-buffer)))

(defun hy-shell--internal? ()
  "Is current buffer for an internal Hy interpreter process?"
  (s-equals? (buffer-name) hy-shell--buffer-name-internal))

(defun hy-shell--format-startup-command ()
  "Format Hy shell startup command."
  (let ((prog (shell-quote-argument hy-shell--interpreter))
        (switches (->> hy-shell--interpreter-args
                     (-map #'shell-quote-argument)
                     (s-join " "))))
    (if (hy-shell--internal?)
        prog
      (format "%s %s" prog switches))))

(defun hy-shell--format-startup-name ()
  "Format the Hy shell process name, for before we actually created it."
  (if (hy-shell--internal?)
      hy-shell--name-internal
    hy-shell--name))

;;;; Creation

(defun hy-shell--make-comint ()
  "Create Hy shell comint process in current-buffer."
  (unless (process-live-p (hy-shell--current-process))
    (-let (((program . switches)
            (split-string-and-unquote (hy-shell--format-startup-command)))
           (name (hy-shell--format-startup-name)))
      (apply #'make-comint-in-buffer name nil program nil switches)

      (unless (derived-mode-p 'inferior-hy-mode)
        (inferior-hy-mode))

      (hy-shell--current-process))))

(defun hy-shell--make-comint-internal ()
  "Run `hy-shell--make-comint' with additional setup for internal processes."
  (let ((hy-shell--enable-font-lock?))
    (-when-let (proc (hy-shell--make-comint))
      (set-process-query-on-exit-flag proc nil)
      proc)))

;;; Sending Text - Transfer in Progress

;; (defun hy-shell--end-of-output? (text)
;;   "Does TEXT contain a prompt, and so, signal end of the output?"
;;   (s-matches? comint-prompt-regexp text))

;; (defun hy-shell--text->comint-text (text)
;;   "Format TEXT before sending to comint."
;;   (if (or (not (string-match "\n\\'" text))
;;           (string-match "\n[ \t].*\n?\\'" text))
;;       (s-concat text "\n")
;;     text))

;; (defun hy-shell--send (text)
;;   "Send TEXT to Hy."
;;   (let ((proc (hy-shell--proc))
;;         (hy-shell--output-in-progress t))
;;     (unless proc
;;       (error "No active Hy process found to send text to."))

;;     (let ((comint-text (hy-shell--text->comint-text text)))
;;       (comint-send-string proc comint-text))))

;;; Font Locking - Transfer In Progress
;;;; New Idea

(defun hy-shell--support-font-lock-input ()
  "Fontify the current line being entered in the Hy shell.

The implementation:
1. A fontification hook is added to `post-command-hook'.
2. The current prompt being entered is replaced with a fontified version.

This seems, and indeed is, an obtuse way to fontify input.
Why don't we just add hy-mode's `font-lock-keywords' into `inferior-hy-mode'?
Then we would fontify the shell output too, which can be arbitrary!

So what if we only fontify input regions?
(actually I'm trying this out right now)
"
  (setq font-lock-defaults
        '(inferior-hy-font-lock-kwds
          nil nil
          (("+-*/.<>=!?$%_&~^:@" . "w"))  ; syntax alist
          nil
          (font-lock-mark-block-function . mark-defun)
          (font-lock-syntactic-face-function  ; Differentiates (doc)strings
           . hy-font-lock-syntactic-face-function)))

  (unless (hy-shell--internal?)
    (font-lock-mode 1))

  ;; (add-hook 'post-command-hook
  ;;           #'hy--shell-fontify-prompt-post-command-hook nil 'local)
  ;; (add-hook 'kill-buffer-hook
  ;;           #'hy--shell-kill-buffer nil 'local)
  )

(setq hy-font-lock--test-comint-rx
      (rx-to-string `(: (group symbol-start
                               (or ,@hy-font-lock--special-names)
                               symbol-end))))

(defun hy-font-lock--test-comint-search (limit)
  (when (re-search-forward hy-font-lock--test-comint-rx limit t)
    (-let* ((md (match-data))
            (start (match-beginning 1))
            ((comint-last-start . comint-last-end) comint-last-prompt))
      (if (or (<= start comint-last-start)
              (hy--in-string-or-comment? (syntax-ppss)))
          (hy-font-lock--test-comint-search limit)
        (goto-char start)
        (forward-sexp)
        (setf (elt md 3) (point))
        (set-match-data md)
        t))))

(setq hy-font-lock--kwds-test-comint (list #'hy-font-lock--test-comint-search
                                          '(1 font-lock-keyword-face t)))
(setq inferior-hy-font-lock-kwds (list hy-font-lock--kwds-test-comint))

;;;; Post-Command-based

;; (defun hy-font-lock--convert-kwd-for-comint (kwd)
;;   (-let (((regex . rest) kwd))
;;     `((lambda (limit)
;;         (-let (((comint-last-start . comint-last-end) comint-last-prompt)
;;                (start (point)))
;;           (when (re-search-forward ,regex limit nil t)
;;             (and (<= start comint-last-start)
;;                  ;; (<= (point) comint-last-end)
;;                  ))))
;;       rest)))

;; (defun hy-font-lock--comint-prompt-check (regex limit)
;;   (-let (((comint-last-start . comint-last-end) comint-last-prompt)
;;          (start (point)))
;;     (when (re-search-forward ,regex limit nil t)
;;       (and (<= start comint-last-start)
;;            ;; (<= (point) comint-last-end)
;;            ))))


;; (defun hy-shell--faces->font-lock-faces (text &optional start-pos)
;;   "Set all 'face in TEXT to 'font-lock-face optionally starting at START-POS."
;;   (let ((pos 0)
;;         (start-pos (or start-pos 0)))
;;     (while (and (/= pos (length text))
;;                 (setq next (next-single-property-change pos 'face text)))
;;       (-let* ((plist (text-properties-at pos text))
;;               ((&plist 'face face) plist))
;;         (set-text-properties (+ start-pos pos) (+ start-pos next)
;;                              (-doto plist
;;                                (plist-put 'face nil)
;;                                (plist-put 'font-lock-face face)))
;;         (setq pos next)))))

;; (defun hy--shell-fontify-prompt-post-command-hook ()
;;   "Fontify just the current line in `hy-shell-buffer' for `post-command-hook'.

;; Constantly extracts current prompt text and executes and manages applying
;; `hy--shell-faces-to-font-lock-faces' to the text."
;;   (-when-let ((_ . last-prompt-end) comint-last-prompt)
;;     (when (and (hy-shell--current-process)
;;                (> (point) last-prompt-end))  ; prompt currently being entered
;;       (let* ((input (buffer-substring-no-properties last-prompt-end (point-max)))

;;              ;; Hide our re-insertion from Emacs internals
;;              (deactivate-mark)
;;              (buffer-undo-list t)

;;              ;; Build the text
;;              (text (hy--shell-with-font-locked-shell-buffer
;;                     (delete-region (line-beginning-position) (point-max))
;;                     (setq font-lock-buffer-pos (point))
;;                     (insert input)
;;                     (font-lock-ensure)
;;                     (buffer-substring font-lock-buffer-pos (point-max)))))
;;         (hy--shell-faces-to-font-lock-faces text last-prompt-end)))))

;;; Jedhy

(defun hy-shell--setup-jedhy ()
  "Stub.")

;;; Notifications

(defun hy-shell--check-installed? ()
  "Warn if `hy-shell--interpreter' is not found, returning non-nil otherwise."
  (cond
   ((executable-find hy-shell--interpreter))
   (hy-shell--notify?
    (prog1 nil
      (message "Hy executable not found. Install or activate a env with Hy.")))))

(defun hy-shell--notify-process-success-internal ()
  (when hy-shell--notify?
    (message "Internal Hy shell process successfully started.")))

;;; inferior-hy-mode
;;;; Components

(defun hy-inferior--support-colorama-output ()
  "Support colorama'd shell output (like errors/traces) with `ansi-color'."
  (ansi-color-for-comint-mode-on)
  (add-to-list 'comint-output-filter-functions #'ansi-color-process-output))

(defun hy-inferior--support-xterm-color ()
  "Support `xterm-color' in shell output."
  (when (fboundp #'xterm-color-filter)
    (add-to-list 'comint-preoutput-filter-functions #'xterm-color-filter)))

(defun hy-inferior--fix-comint-input-history-breaking ()
  (advice-add #'comint-previous-input :before
              (lambda (&rest args) (setq-local comint-stored-incomplete-input ""))))

;; TODO Internal process startup handling and the pyvenv hook
;; (defun hy--mode-setup-inferior ()
;;   ;; (add-to-list 'company-backends 'company-hy)
;;   (setenv "PYTHONIOENCODING" "UTF-8")

;;   (run-hy-internal)
;;   (add-hook 'pyvenv-post-activate-hooks 'run-hy-internal nil t))

;;;; Mode Declaration

;;;###autoload
(define-derived-mode inferior-hy-mode comint-mode "Inferior Hy"
  "Major mode for Hy inferior process."
  ;; Comint config
  (setq mode-line-process '(":%s"))
  (setq-local indent-tabs-mode nil)
  (setq-local comint-prompt-read-only t)
  (setq-local comint-prompt-regexp (rx bol "=>" space))
  (hy-inferior--fix-comint-input-history-breaking)

  ;; Instantiate filters
  (setq-local comint-preoutput-filter-functions nil)
  (setq-local comint-output-filter-functions nil)

  ;; Build Filters
  (hy-inferior--support-colorama-output)
  (hy-inferior--support-xterm-color)
  (when hy-shell--enable-font-lock?
    (hy-shell--support-font-lock-input)))

;;; Commands
;;;; Killing

(defun hy-shell--kill ()
  "Kill the Hy interpreter process."
  (interactive)

  (hy-shell--with-live
    (kill-buffer (current-buffer))))

(defun hy-shell--kill-internal ()
  "Kill the internal Hy interpreter process."
  (interactive)

  (hy-shell--with-internal-live
    (kill-buffer (current-buffer))))

(defun hy-shell--kill-all ()
  "Kill all Hy interpreter processes."
  (interactive)

  (hy-shell--kill)
  (hy-shell--kill-internal))

;;;; Running

(defun run-hy-internal ()
  "Startup the internal Hy interpreter process."
  (interactive)

  (hy-shell--with-internal
    (hy-shell--setup-jedhy)
    (hy-shell--notify-process-success-internal)))

(defun run-hy ()
  "Startup and/or switch to a Hy interpreter process."
  (interactive)

  (hy-shell--with
    (switch-to-buffer-other-window (current-buffer))))

;;; Provide:

(provide 'hy-shell)
