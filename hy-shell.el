;;; hy-shell.el --- Shell and Process Support -*- lexical-binding: t -*-

;; Copyright © 2013 Julien Danjou <julien@danjou.info>
;;           © 2017-2019 Eric Kaschalk <ekaschalk@gmail.com>
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

;; This file implements `inferior-hy-mode', commands to send and get text
;; from Hy interpreters, and other repl components.

;; See `hy-jedhy.el' which builds on these commands to support IDE components.

;;; Code:

(require 'hy-base)

(require 'hy-font-lock)

;;; Configuration
;;;; Configured

(defvar hy-shell--interpreter "hy"
  "Default Hy interpreter name.")

(defvar hy-shell--interpreter-args '("--spy")
  "Default argument list to pass to the Hy interpreter.")

(defvar hy-shell--enable-font-lock? t
  "Whether the shell should font-lock repl prompt input.")

(defvar hy-shell--notify? t
  "Allow Hy to message on failure to find Hy, instantiation, shutdown, etc?")

(defvar hy-shell--redirect-timeout 0.5
  "Seconds (float) to allow redirection commands to complete before quitting.")

;;;; Managed

(defconst hy-shell--name "Hy"
  "The name to use for the Hy interpreter process.")

(defconst hy-shell--name-internal (format "%s Internal" hy-shell--name)
  "The name to use for the internal Hy interpreter process.")

(defconst hy-shell--buffer-name (s-concat "*" hy-shell--name "*")
  "The buffer name to use for the Hy interpreter process.")

(defconst hy-shell--buffer-name-internal (s-concat "*" hy-shell--name-internal "*")
  "The buffer name to use for the internal Hy interpreter process.")

(defvar hy-shell--redirect-output-buffer " *Hy Comint Redirect Buffer"
  "The buffer name to use for comint redirection of text sending commands.")

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
  (get-buffer-process hy-shell--buffer-name))

(defun hy-shell--live-internal? ()
  "Is the internal Hy intereprter process alive?"
  (get-buffer-process hy-shell--buffer-name-internal))

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

;;;; Creation

(defun hy-shell--make-comint ()
  "Create Hy shell comint process in current-buffer."
  (unless (process-live-p (hy-shell--current-process))
    (-let (((program . switches)
            (split-string-and-unquote (hy-shell--format-startup-command)))
           (name
            (if (hy-shell--internal?) hy-shell--name-internal hy-shell--name)))
      (apply #'make-comint-in-buffer name nil program nil switches)

      (unless (derived-mode-p 'inferior-hy-mode)
        (inferior-hy-mode))

      ;; Get shell's initial output/prompt
      (accept-process-output (hy-shell--current-process) 0.5)

      (hy-shell--current-process))))

(defun hy-shell--make-comint-internal ()
  "Run `hy-shell--make-comint' with additional setup for internal processes."
  (let ((hy-shell--enable-font-lock?))
    (-when-let (proc (hy-shell--make-comint))
      (set-process-query-on-exit-flag proc nil)
      proc)))

;;; Redirected Sending
;;;; Commentary

;; Maybe in the future I build an nrepl or lsp implementation. Until that day,
;; interacting with Hy's running processes programatically is done through the
;; `hy-shell--redirect-send' and friends commands.

;; They are rewrites of some components of comint's redirection commands.
;; The redirection add-on for comint was developed to run SQL on a process
;; with state. Similarly, we maintain state in jedhy's namespace. There
;; are better, but more advanced, solutions. The one chosen should allow a
;; pretty quick, and !easily testable!, integration of jedhy. It also allows
;; some fancier things w.r.t shell output transformations and fontifying.

;; The commands are rewritten because 1. we don't need all the options 2. we
;; require a timeout during the accept process output 3. we have some macros
;; that simplify things 4. easy to test this way.

;;;; Implementation

(defun hy-shell--redirect-check-prompt-regexp ()
  "Avoid infinite loop in redirect if `comint-prompt-regexp' badly defined."
  (when comint-redirect-perform-sanity-check
    (save-excursion
	    (goto-char (point-max))
	    (or (re-search-backward comint-prompt-regexp nil t)
		      (error "No prompt found or `comint-prompt-regexp' not set properly")))))

(defun hy-shell--redirect-send-1 (text)
  "Internal implementation of `comint-redirect-send-command-to-process'.

Expected to be called within a Hy interpreter process buffer."
  (hy-shell--redirect-check-prompt-regexp)

  (let ((buffer (current-buffer))
        (output-buffer hy-shell--redirect-output-buffer)
        (process (hy-shell--current-process))
        (timeout hy-shell--redirect-timeout))
    ;; Setup local vars for the filter, temporarily overwrite comint filters
    (comint-redirect-setup output-buffer buffer comint-prompt-regexp)
    (add-function :around (process-filter process) #'comint-redirect-filter)

    (process-send-string buffer (s-concat text "\n"))
    (while (and (null comint-redirect-completed)
		            (accept-process-output process timeout)))))

(defun hy-shell--redirect-send (text)
  "Send TEXT to Hy interpreter, capturing and removing the output."
  (with-current-buffer (get-buffer-create hy-shell--redirect-output-buffer)
    (erase-buffer)
    (hy-shell--with
      (hy-shell--redirect-send-1 text))
    (s-chomp (buffer-substring-no-properties (point-min) (point-max)))))

(defun hy-shell--redirect-send-internal (text)
  "Send TEXT to internal Hy interpreter, capturing and removing the output."
  (with-current-buffer (get-buffer-create hy-shell--redirect-output-buffer)
    (erase-buffer)
    (hy-shell--with-internal
      (hy-shell--redirect-send-1 text))
    (s-chomp (buffer-substring-no-properties (point-min) (point-max)))))

;;; Sending Text
;;;; Interface

(defun hy-shell--send (text)
  "Send TEXT to Hy interpreter, starting up if needed."
  (hy-shell--with
    (let ((hy-shell--output-in-progress t)
          (proc (hy-shell--current-process)))
      (comint-send-string proc text))))

(defun hy-shell--send-internal (text)
  "Send TEXT to Hy interpreter, starting up if needed."
  (hy-shell--with-internal
    (let ((hy-shell--output-in-progress t)
          (proc (hy-shell--current-process)))
      (comint-send-string proc text))))

;;;; Macros

(defmacro hy-shell--eval-1 (text)
  "Internal implementation of interactive eval commands."
  (declare (indent 0))
  (let ((text-sym (gensym)))
    `(-when-let (,text-sym ,text)
       (run-hy)
       (hy-shell--with-live
         ;; TODO Force the initial/end cases in a nicer way if possible
         (hy-shell--send "\n")
         (hy-shell--send ,text-sym)
         (hy-shell--send "\n")))))

;;;; Commands

(defun hy-shell-eval-current-form ()
  "Send form containing point to the Hy interpreter, starting up if needed."
  (interactive)
  (hy-shell--eval-1
    (hy--current-form-string)))

(defun hy-shell-eval-last-sexp ()
  "Send the last sexp to the Hy interpreter, starting up if needed."
  (interactive)
  (hy-shell--eval-1
    (hy--last-sexp-string)))

(defun hy-shell-eval-region ()
  "Send region to the Hy interpreter, starting up if needed."
  (interactive)
  (when (and (region-active-p) (not (region-noncontiguous-p)))
    (hy-shell--eval-1
      (buffer-substring (region-beginning) (region-end)))))

(defun hy-shell-eval-buffer ()
  "Send the current buffer to the Hy interpreter, starting up if needed."
  (interactive)
  (hy-shell--eval-1
    (buffer-string)))

;;; Notifications

(defun hy-shell--notify (msg)
  "`message' MSG if `hy-shell--notify?' is non-nil."
  (when hy-shell--notify?
    (message msg)))

(defun hy-shell--check-installed? ()
  "Warn if `hy-shell--interpreter' is not found, returning non-nil otherwise."
  (cond
   ((executable-find hy-shell--interpreter))
   ((prog1 nil
      (hy-shell--notify "Hy cmd not found. Install or activate a env with Hy.")))))

;;; inferior-hy-mode
;;;; Colorings

(defun hy-inferior--support-font-locking-input ()
  "Fontify the current line being entered in the Hy shell.

The solution implemented is my own and was interesting enough to warrant
a blog post: http://www.modernemacs.com/post/comint-highlighting/."
  (unless (hy-shell--internal?)
    (setq font-lock-defaults
          '(inferior-hy-font-lock-kwds
            nil nil
            (("+-*/.<>=!?$%_&~^:@" . "w"))  ; syntax alist
            nil
            (font-lock-mark-block-function . mark-defun)
            (font-lock-syntactic-face-function  ; Differentiates (doc)strings
             . hy-font-lock-syntactic-face-function)))
    (setq-local syntax-propertize-function #'hy-syntax-propertize-function)
    (font-lock-mode 1)))

(defun hy-inferior--support-colorama-output ()
  "Support colorama'd shell output (like errors/traces) with `ansi-color'."
  (ansi-color-for-comint-mode-on)
  (add-hook 'comint-output-filter-functions #'ansi-color-process-output))

(defun hy-inferior--support-xterm-color ()
  "Support `xterm-color' in shell output."
  (when (fboundp #'xterm-color-filter)  ; not installed by default
    (add-hook 'comint-preoutput-filter-functions #'xterm-color-filter)))

;;;; Comint Configurations

(defun hy-inferior--fix-comint-input-history-breaking ()
  "Temp resolves comint's history sometimes failing, no side effects I think."
  (advice-add #'comint-previous-input :before
              (lambda (&rest args) (setq-local comint-stored-incomplete-input ""))))

;;;; Mode Declaration

;;;###autoload
(define-derived-mode inferior-hy-mode comint-mode "Inferior Hy"
  "Major mode for Hy inferior process."
  (setenv "PYTHONIOENCODING" "UTF-8")

  (setq-local indent-tabs-mode nil)
  (setq-local comint-prompt-read-only t)
  (setq-local comint-prompt-regexp (rx bol "=>" space))
  (hy-inferior--fix-comint-input-history-breaking)

  (setq-local comint-preoutput-filter-functions nil)
  (setq-local comint-output-filter-functions nil)

  (hy-inferior--support-colorama-output)
  (hy-inferior--support-xterm-color)

  (when hy-shell--enable-font-lock?
    (hy-inferior--support-font-locking-input)))

(define-key inferior-hy-mode-map (kbd "C-c C-z")
  (lambda () (interactive) (other-window -1)))

;;; Commands
;;;; Killing

(defun hy-shell--kill ()
  "Kill the Hy interpreter process."
  (interactive)

  (-when-let (buff (get-buffer hy-shell--buffer-name))
    (kill-buffer buff)))

(defun hy-shell--kill-internal ()
  "Kill the internal Hy interpreter process."
  (interactive)

  (-when-let (buff (get-buffer hy-shell--buffer-name-internal))
    (kill-buffer buff)))

(defun hy-shell--kill-all ()
  "Kill all Hy interpreter processes."
  (interactive)

  (hy-shell--kill)
  (hy-shell--kill-internal))

;;;; Running

;;;###autoload
(defun run-hy ()
  "Startup and/or switch to a Hy interpreter process."
  (interactive)

  (hy-shell--with
    (switch-to-buffer-other-window (current-buffer))))

;;; Provide:

(provide 'hy-shell)

;;; hy-shell.el ends here
