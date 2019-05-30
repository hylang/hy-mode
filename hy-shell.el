;;; hy-shell.el --- Shell and Process Support -*- lexical-binding: t -*-

;; Copyright © 2013-2016 Julien Danjou <julien@danjou.info>
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

;;; Code:

(require 'hy-base)

(require 'hy-font-lock)

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

(defvar hy-shell--redirect-timeout 0.5
  "Seconds to allow for redirection commands to complete before quitting.")

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
;;;; Old

;; TODO Tailor `hy-shell--redirect-send' to handle the extra requirements here
;; (defun hy-shell--send-inhibit-output (string &optional process internal)
;;   "Send TEXT to Hy interpreter inhibiting output, starting up if needed."
;;   (hy-shell--with
;;     (let ((inhibit-quit t)
;;           (hy-shell--output-in-progress t)
;;           (proc (hy-shell--current-process)))
;;       (unless (with-local-quit
;;                 (comint-send-string proc text)
;;                 (while hy-shell--output-in-progress
;;                   (accept-process-output process))
;;                 t)
;;         (comint-interrupt-subjob)))))

;; (defun hy-shell--end-of-output? (text)
;;   "Does TEXT contain a prompt, and so, signal end of the output?"
;;   (s-matches? comint-prompt-regexp text))

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

;;;; Commands

(defmacro hy-shell--eval-1 (text)
  (let ((text-sym (gensym)))
    `(-when-let (,text-sym ,text)
       (run-hy)
       (hy-shell--with-live (hy-shell--send ,text-sym)))))

(defun hy-shell-eval-current-form ()
  "Send form containing point to the Hy interpreter, starting up if needed."
  (interactive)
  (hy-shell--eval-1 (hy--current-form-string)))

(defun hy-shell-eval-last-sexp ()
  (interactive)
  (hy-shell--eval-1 (hy--last-sexp-string)))

;; FIXME
(defun hy-shell-eval-region ()
  (interactive)
  (when (and (region-active-p) (not (region-noncontiguous-p)))
    (hy-shell--eval-1 (buffer-substring (region-beginning) (region-end)))))

;; FIXME
(defun hy-shell-eval-buffer ()
  (interactive)
  (when (and (region-active-p) (not (region-noncontiguous-p)))
    (hy-shell--eval-1 (buffer-string))))

;;; Jedhy
;;;; Code

;; TODO Redirected sending of multiple lines needs to concatenate the outputs

(defvar-local hy-shell--jedhy-running? nil
  "Was `jedhy' successfully started up in the current buffer?")

(defconst hy-shell--jedhy-success-text "'Started jedhy'"
  "Text identifying successful startup of jedhy.")

(defconst hy-shell--jedhy-fail-text "'Failed to start jedhy'"
  "Text identifying failure to startup jedhy.")

(defconst hy-shell--jedhy-setup-code
  "(try (do (import jedhy jedhy.api) (setv --JEDHY (jedhy.api.API)) \"Started jedhy\") (except [e Exception] \"Failed to start jedhy\"))"
  "Text to send to internal Hy process to setup `jedhy', via --JEDHY.")

(defconst hy-shell--jedhy-reset-namespace-code
  "(--JEDHY.set-namespace :locals- (locals) :globals- (globals) :macros- --macros--)"
  "Text to send to make Jedhy's namespace current.")

(defun hy-shell--jedhy-installed? () "Stub." t)

(defun hy-shell--startup-jedhy ()
  "Startup jedhy and notify its status, returning non-nil if successful."
  (hy-shell--with-internal
    (unless hy-shell--jedhy-running?
      (let ((status (hy-shell--redirect-send-internal hy-shell--jedhy-setup-code)))
        (if (s-equals? status hy-shell--jedhy-success-text)
            (prog1 t
              (when hy-shell--notify? (message "Jedhy successfully started"))
              (setq-local hy-shell--jedhy-running? t))
          (prog1 nil
            (when hy-shell--notify? (message "Jedhy failed to start"))
            (setq-local hy-shell--jedhy-running? nil)))))))

(defun hy-shell--reset-namespace ()
  "Make Jedhy's namespace current."
  ;; TODO should include the default namespace stuff like itertools
  ;; like how I do it in jedhy.
  (when hy-shell--jedhy-running?
    (hy-shell--redirect-send-internal hy-shell--jedhy-reset-namespace-code)))

;;; Company
;;;; Symbol Extraction

(defun hy-shell--method-call? (symbol)
  "Is SYMBOL a method call in Hy?"
  (s-starts-with? "." symbol))

(defun hy-shell--quickfix-eldoc-dot-dsl-syntax-errors (text)
  "Quick fix to address parsing an incomplete dot-dsl."
  (if (< 1 (-> text s-lines length))
      ""
    text))

(defun hy-shell--get-inner-symbol ()
  "Get inner symbol for point, completing Hy's method-dot DSL if applicable."
  (save-excursion
    (-when-let (inner-symbol (and (hy--goto-inner-sexp (syntax-ppss))
                                  (not (-contains? '(?\[ ?\{) (char-before)))
                                  (thing-at-point 'symbol)))
      (if (hy-shell--method-call? inner-symbol)
          (when (ignore-errors (forward-sexp) (forward-whitespace 1) t)
            (pcase (char-after)
              ;; Can't send just .method to eldoc
              ((or ?\) ?\s ?\C-j) nil)

              ;; Dot dsl doesn't work on literals
              (?\[ (s-concat "list" inner-symbol))
              (?\{ (s-concat "dict" inner-symbol))
              (?\" (s-concat "str" inner-symbol))

              ;; Otherwise complete the dot dsl
              (_ (s-concat (thing-at-point 'symbol) inner-symbol))))
        inner-symbol))))

;;;; Output Formats

(defun hy-shell--format-output-str (output)
  "Format OUTPUT given as a string."
  (->> output
     (s-chop-prefixes '("'" "\""))
     (s-chop-suffixes '("'" "\""))))

(defun hy-shell--format-output-tuple (output)
  "Format OUTPUT given as a tuple."
  (unless (s-equals? "()" output)
    (->> output
       (s-replace-all '(("'" . "")
                        (",)" . "")  ; one element list case
                        ("(" . "")
                        (")" . "")))
       (s-split ", "))))  ; comma is a valid token so can't replace it

;;;; Fontifying

(defun hy-shell--fontify-text (text regexp &rest faces)
  "Fontify portions of TEXT matching REGEXP with FACES."
  (when text
    (-each (s-matched-positions-all regexp text)
      (-lambda ((start . end))
        (-each faces
          (lambda (face)
            (add-face-text-property start end face nil text)))))))

(defun hy-shell--fontify-eldoc (text)
  "Fontify eldoc TEXT."
  (let ((kwd-rx
         (rx string-start (1+ (not (any space ":"))) ":"))
        (unpack-rx
         (rx (or "#*" "#**")))
        (kwargs-rx
         (rx symbol-start "&" (1+ word)))
        (quoted-args-rx
         (rx "`" (1+ (not space)) "`")))
    (hy--fontify-text text kwd-rx 'font-lock-keyword-face)
    (hy--fontify-text text unpack-rx 'font-lock-keyword-face)
    (hy--fontify-text text kwargs-rx 'font-lock-type-face)
    (hy--fontify-text text quoted-args-rx 'font-lock-constant-face 'bold-italic))
  text)

;;;; Jedhy Interface

(defun hy-shell--prefix-str->candidates (prefix-str)
  "Get company candidates for a PREFIX-STR."
  (unless (hy-shell--method-call? prefix-str)
    (-some->>
     prefix-str
     (format "(--JEDHY.complete \"%s\")")
     hy-shell--redirect-send-internal
     hy-shell--format-output-tuple)))

(defun hy-shell--candidate-str->annotation (candidate-str)
  "Get company annotation for a CANDIDATE-STR."
  (-some->>
   candidate-str
   (format "(--JEDHY.annotate \"%s\")")
   hy-shell--redirect-send-internal
   hy-shell--format-output-str))

(defun hy-shell--candidate-str->eldoc (candidate-str)
  "Get eldoc docstring for a CANDIDATE-STR."
  ;; TODO Eldoc gives "builtin immutable sequence" on compiler candidates
  (-some->>
   candidate-str
   (format "(--JEDHY.docs \"%s\")")
   hy-shell--redirect-send-internal
   hy-shell--format-output-str
   hy-shell--quickfix-eldoc-dot-dsl-syntax-errors
   hy-shell--fontify-eldoc))

;;;; Commands

(defun hy-eldoc-documentation-function ()
  "Drives `eldoc-mode', retrieves eldoc msg string for inner-most symbol."
  (hy-shell--candidate-str->eldoc (hy-shell--get-inner-symbol)))

(defun company-hy (command &optional prefix-or-candidate-str &rest ignored)
  (interactive (list 'interactive))

  (cl-case command
    (prefix (company-grab-symbol))
    (candidates (hy-shell--prefix-str->candidates
                 prefix-or-candidate-str))
    (annotation (hy-shell--candidate-str->annotation
                 prefix-or-candidate-str))
    (meta (hy-shell--candidate-str->eldoc
           prefix-or-candidate-str))))

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

;; SETUP
;; pip install -e "~/dev/jedhy/"
;; (import jedhy jedhy.api)
;; (setv --JEDHY (jedhy.api.API))

;; (hy-shell--startup-jedhy)
;; (hy-shell--reset-namespace)

;; (hy-shell--prefix-str->candidates "it.")
;; (hy-shell--prefix-str->candidates "itertools.-")
;; (hy-shell--candidate-str->annotation "try")
;; (hy-shell--candidate-str->eldoc "itertools")

;; (spacemacs|add-company-backends
;;   :backends company-hy
;;   :modes hy-mode inferior-hy-mode)

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

;;;###autoload
(defun run-hy-internal ()
  "Startup the internal Hy interpreter process."
  (interactive)

  (hy-shell--with-internal
    (when (hy-shell--startup-jedhy)
      (hy-shell--notify-process-success-internal))))

;;;###autoload
(defun run-hy ()
  "Startup and/or switch to a Hy interpreter process."
  (interactive)

  (hy-shell--with
    (switch-to-buffer-other-window (current-buffer))))

;;; Provide:

(provide 'hy-shell)
