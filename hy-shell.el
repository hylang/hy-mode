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

;; This file implements `inferior-hy-mode', commands to send and get text
;; from Hy interpreters, `company-mode' support, `eldoc-mode' support, and
;; documentation introspection of symbol at point.

;; Interfaces with jedhy, a Hy library I've written to support completion,
;; documentation lookup, and other introspection code.

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

(defvar hy-shell--doc-lookup-buffer " *Hy Doc Lookup Buffer"
  "The buffer name to use for documentation lookups.")

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
  "(import hy [hy.core.language [*]] [hy.core.macros [*]]) (require [hy.extra.anaphoric [*]]) (try (do (import jedhy jedhy.api) (setv --JEDHY (jedhy.api.API)) \"Started jedhy\") (except [e Exception] \"Failed to start jedhy\"))"
  "Text to send to internal Hy process to setup `jedhy', via --JEDHY.")

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

;;;; Namespace Updating

;; TODO Will do this automatically when I figure out a good way to do it

;; TODO Why is set-namespace version not working?
(defconst hy-shell--jedhy-reset-namespace-code
  ;; "(--JEDHY.set-namespace :locals- (locals) :globals- (globals) :macros- --macros--)"
  "(setv --JEDHY (jedhy.api.API :locals- (locals) :globals- (globals) :macros- --macros--))"
  "Text to send to make Jedhy's namespace current.")

(defconst hy-shell--import-rgx
  (rx "(" (0+ space)
      (or "import" "require" "sys.path.extend"))
  "A *temporary* regex used to extract import forms for updating IDE features.")

(defun hy-shell-update-imports ()
  "Send imports/requires to the current internal process and updating namespace.

This is currently done manually as I'm not sure of the consequences of doing
so automatically through eg. regular intervals. Sending the imports allows
Eldoc/Company to function on packages like numpy/pandas, even if done via an
alias like (import [numpy :as np]).

Not bound atm as this is temporary, run via M-x or bind yourself."
  (interactive)
  (save-excursion
    (goto-char (point-min))

    (while (re-search-forward hy-shell--import-rgx nil t)
      (let ((text (s-join " " (s-lines (hy--current-form-string)))))
        (hy-shell--redirect-send-internal text)))

    (hy-shell--redirect-send-internal hy-shell--jedhy-reset-namespace-code)))

;;; Company and Eldoc
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
  (-some->>
   candidate-str
   (format "(--JEDHY.docs \"%s\")")
   hy-shell--redirect-send-internal
   hy-shell--format-output-str
   hy-shell--quickfix-eldoc-dot-dsl-syntax-errors
   hy-shell--fontify-eldoc))

(defun hy-shell--candidate-str->full-docs (candidate-str)
  "Get full, multi-line docs for a CANDIDATE-STR."
  (-some->>
   candidate-str
   (format "(--JEDHY.full-docs \"%s\")")
   hy-shell--redirect-send-internal
   hy-shell--format-output-str
   s-chomp
   hy-shell--fontify-first-docs-line
   hy-shell--format-describe-output))

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

;;; Describe thing at point

(defun hy-shell--docs-for-thing-at-point ()
  (hy-shell--candidate-str->full-docs (thing-at-point 'symbol)))

(defun hy-shell--fontify-first-docs-line (output)
  "Fontify only the first line of jedhy OUTPUT accordding to eldoc."
  (when output
    (-let (((leader . rest) (s-lines output)))
      (s-join "\n"
              (cons (hy-shell--fontify-eldoc leader)
                    rest)))))

(defun hy-shell--format-describe-output (output)
  "Converts escaped newlines to true newlines."
  (let ((kwarg-newline-regexp (rx ","
                                  (1+ (not (any "," ")")))
                                  (group-n 1 "\\\n")
                                  (1+ (not (any "," ")"))))))
    (-some-->
     output
     (s-replace "\\n" "\n" it)
     (replace-regexp-in-string kwarg-newline-regexp "newline" it nil t 1))))

(defun hy-describe-thing-at-point ()
  "Describe symbol at point with help popup buffer.

Retrieves full documentation, with firstline formatted same as eldoc, in a
popup buffer.

Does not (yet) complete the dot-dsl like Eldoc does currently.

Spacemacs users maybe be familiar with this functionality via
shift-K keybinding that executes `spacemacs/evil-smart-doc-lookup'."
  (interactive)
  (-when-let (text (hy-shell--docs-for-thing-at-point))
    (unless (s-blank-str? text)
      (with-current-buffer (get-buffer-create hy-shell--doc-lookup-buffer)
        (erase-buffer)
        (switch-to-buffer-other-window hy-shell--doc-lookup-buffer)

        (insert text)

        (when (< 1 (length (s-lines text)))
          (goto-char (point-min))
          (forward-line)
          (newline)
          (insert "------")
          (fill-region (point) (point-max)))

        (goto-char (point-min))

        ;; TODO This can be in a better way I'm assuming
        (local-set-key "q" #'quit-window)
        (when (fboundp #'evil-local-set-key)
          (evil-local-set-key 'normal "q" #'quit-window))))))

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
  "Startup internal Hy interpreter process, enabling jedhy for `company-mode'."
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
