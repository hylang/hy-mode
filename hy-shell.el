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

;;; Process Management
;;;; Utilities

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
           (name
            (hy-shell--format-startup-name)))
      (apply #'make-comint-in-buffer name nil program nil switches)
      (unless (derived-mode-p 'inferior-hy-mode) (inferior-hy-mode))
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

;;; Commands
;;;; Killing

(defun hy-shell--kill ()
  "Kill the Hy interpreter process."
  (interactive)

  (hy-shell--with
    (kill-buffer (current-buffer))))

(defun hy-shell--kill-internal ()
  "Kill the internal Hy interpreter process."
  (interactive)

  (hy-shell--with-internal
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
