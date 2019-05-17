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

(defvar hy-shell--interpreter-args "--spy"
  "Default argument string to pass to the Hy interpreter.")

(defvar hy-shell--startup-internal-process? t
  "Should an internal process startup for use by ide components?")

(defvar hy-shell--font-lock-enable t
  "Whether the shell should font-lock the current line.")

(defvar hy-shell--notify? t
  "Should Hy message on successful instantiation, shutdown, etc?")

;;;; Constants

(defconst hy-shell--buffer-name "Hy"
  "Default buffer name for Hy interpreter.")

(defconst hy-shell--buffer-name-internal "Hy Internal"
  "Default buffer name for the internal Hy process.")

;;;; Managed

(defvar hy-shell--buffer nil
  "The current shell buffer for Hy.")

(defvar hy-shell--buffer-internal nil
  "The current internal shell buffer for Hy.")

(defvar hy-shell--output-in-progress nil
  "Whether we are waiting for output in `hy-shell-send-string-no-output'.")

;;; Fundamentals
;;;; Accessing

(defun hy-shell--installed? ()
  "Is the command `hy-shell--interpreter' available?"
  (executable-find hy-shell--interpreter))

(defun hy-shell--get-buffer-create ()
  "Get or create `hy-shell--buffer'."
  (setq hy-shell--buffer
        (get-buffer-create hy-shell--buffer-name)))

(defun hy-shell--get-buffer-create-internal ()
  "Get or create `hy-shell--buffer-internal'."
  (setq hy-shell--buffer-internal
        (get-buffer-create hy-shell--buffer-name-internal)))

(defun hy-shell--proc ()
  "Process corr. to `hy-shell--buffer-name'."
  (get-process hy-shell--buffer-name))

(defun hy-shell--proc-internal ()
  "Process corr. to `hy-shell--buffer-name-internal'."
  (get-process hy-shell--buffer-name-internal))

;;;; Formatting

(defun hy-shell--format-proc-name (buffer-name)
  "Enclose BUFFER-NAME with astericks to follow process naming conventions."
  (s-concat "*" buffer-name "*"))

(defun hy-shell--format-startup-command ()
  "Format Hy shell startup command."
  (format "%s %s"
          (shell-quote-argument hy-shell--interpreter)
          hy-shell--interpreter-args))

(defun hy-shell--format-startup-command-internal ()
  "Format Hy shell's internal startup command."
  (shell-quote-argument hy-shell--interpreter))

;;;; Status

(defun hy-shell--live? ()
  "Is Hy's shell alive?"
  (process-live-p (hy-shell--proc)))

(defun hy-shell--buffer-live? ()
  "Is Hy's shell buffer alive?"
  (when hy-shell--buffer
    (buffer-live-p hy-shell--buffer)))

(defun hy-shell--live-internal? ()
  "Is Hy's internal process alive?"
  (process-live-p (hy-shell--proc-internal)))

(defun hy-shell--buffer-live-internal? ()
  "Is Hy's shell buffer alive?"
  (when hy-shell--buffer-internal
    (buffer-live-p hy-shell--buffer-internal)))

;;; Macros

(defmacro hy-shell--with-shell (&rest forms)
  "Execute FORMS in the shell buffer."
  (let ((shell-proc (gensym)))
    `(let ((,shell-proc (hy-shell--proc)))
       (with-current-buffer (process-buffer ,shell-proc)
         ,@forms))))

(defmacro hy-shell--with-shell-internal (&rest forms)
  "Execute FORMS in the internal shell buffer."
  (let ((shell-proc (gensym)))
    `(let ((,shell-proc (hy-shell--proc-internal)))
       (with-current-buffer (process-buffer ,shell-proc)
         ,@forms))))

;;; Killing

(defun hy-shell--kill ()
  "Kill `hy-shell--buffer'."
  (when (hy-shell--buffer-live?)
    (kill-buffer hy-shell--buffer)
    (when (derived-mode-p 'inferior-hy-mode)
      (setq hy-shell--buffer nil))))

(defun hy-shell--kill-internal ()
  "Kill `hy-shell--buffer-internal'."
  (when (hy-shell--buffer-live-internal?)
    (kill-buffer hy-shell--buffer-internal)
    (when (derived-mode-p 'inferior-hy-mode)
      (setq hy-shell--buffer-internal nil))))

;;; Sending Text

(defun hy-shell--end-of-output? (text)
  "Does TEXT contain a prompt, and so, signal end of the output?"
  (s-matches? comint-prompt-regexp text))

(defun hy-shell--text->comint-text (text)
  "Format TEXT before sending to comint."
  (if (or (not (string-match "\n\\'" text))
          (string-match "\n[ \t].*\n?\\'" text))
      (s-concat text "\n")
    text))

(defun hy-shell--send (text)
  "Send TEXT to Hy."
  (let ((proc (hy-shell--proc))
        (hy-shell--output-in-progress t))
    (unless proc
      (error "No active Hy process found to send text to."))

    (let ((comint-text (hy-shell--text->comint-text text)))
      (comint-send-string proc comint-text))))

;;; Creating

(defun hy-shell--make-comint-in-buffer (cmd buffer-name)
  (-let (((program . switches) (split-string-and-unquote cmd))
         (proc-name (hy-shell--format-proc-name buffer-name)))
    (apply 'make-comint-in-buffer buffer-name proc-name program nil switches)))

(defun hy-shell--make-comint ()
  (unless (hy-shell--live?)
    (-when-let* ((cmd (hy-shell--format-startup-command))
                 (buffer (hy-shell--make-comint-in-buffer proc-name)))
      (setq hy-shell--buffer buffer)

      (with-current-buffer buffer
        (unless (derived-mode-p 'inferior-hy-mode)
          (inferior-hy-mode)))

      buffer)))

(defun hy-shell--make-comint-internal ()
  (unless (hy-shell--live-internal?)
    (-when-let* ((cmd (hy-shell--format-startup-command-internal))
                 (buffer (hy-shell--make-comint-in-buffer
                          cmd hy-shell--buffer-name-internal))
                 (process (get-buffer-process buffer)))
      (setq hy-shell--buffer-internal buffer)
      (set-process-query-on-exit-flag process nil)

      (with-current-buffer buffer
        (unless (derived-mode-p 'inferior-hy-mode)
          (inferior-hy-mode)))

      buffer)))

;;; Jedhy

(defun hy-shell--setup-jedhy ()
  "Stub.")

;;; Running
;;;; Utilities

(defun hy-shell--warn-whether-installed ()
  (unless (hy-shell--installed?)
    (message "Hy executable not found. Install or activate a env with Hy.")))

(defun hy-shell--notify-process-success-internal ()
  (when hy-shell--notify?
    (message "Internal Hy shell process successfully started.")))

(defun hy-shell--display ()
  (when hy-shell--buffer
    (display-buffer hy-shell--buffer)))

;;;; Commands

(defun run-hy-internal ()
  (interactive)

  (hy-shell--warn-whether-installed)
  (when (hy-shell--make-comint-internal)
    (hy-shell--setup-jedhy)
    (hy-shell--notify-process-success-internal)))

(defun run-hy ()
  (interactive)

  (hy-shell--warn-whether-installed)
  (when (hy-shell--make-comint)
    (hy-shell--display)))

;;; Provide:

(provide 'hy-shell)
