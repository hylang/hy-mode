;;; hy-mode.el --- Major mode for Hylang -*- lexical-binding: t -*-

;; Copyright © 2013 Julien Danjou <julien@danjou.info>
;;           © 2017 Eric Kaschalk <ekaschalk@gmail.com>
;;
;; Authors: Julien Danjou <julien@danjou.info>
;;          Eric Kaschalk <ekaschalk@gmail.com>
;; URL: http://github.com/hylang/hy-mode
;; Version: 1.0
;; Keywords: languages, lisp, python
;; Package-Requires: ((dash "2.13.0") (dash-functional "1.2.0") (s "1.11.0") (emacs "24"))

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

;; Provides font-lock, indentation, navigation, autocompletion, and other
;; features for working productively in Hy (http://hylang.org).

;; Hy is a lisp embedded in Python.

;;; Code:

(require 'hy-base)

(require 'hy-font-lock)
;; (require 'hy-shell)  ; In progress

(defgroup hy-mode nil
  "A mode for Hy"
  :prefix "hy-mode-"
  :group 'applications)

;;; Configuration
;;;; Inferior shell

(defvar hy-shell-internal? t
  "Should internal hy process be initiated for IDE components?")

(defconst hy-shell-interpreter "hy"
  "Default Hy interpreter name.")

(defvar hy-shell-interpreter-args "--spy"
  "Default arguments for Hy interpreter.")

(defvar hy-shell-use-control-codes? nil
  "Append `--control-codes' flag to `hy-shell-interpreter-args'?

Requires recent version of Hy and `hy-shell-interpreter-args' to contain `--spy'.
Keep nil unless using specific Hy branch.")

(defvar hy-shell-spy-delim ""
  "If using `--spy' interpreter arg then delimit spy ouput by this string.")

;;;; Highlighting

(defvar hy-font-lock-highlight-percent-args? t
  "Whether to highlight '%i' symbols in Hy's clojure-like syntax for lambdas.")

;;;; Indentation

(defvar hy-indent--exactly
  '("when" "unless"
    "for" "for*" "for/a" "for/a*"
    "while"
    "except" "catch")
  "Symbols that will have following lines indented +1 when matched.")

(defvar hy-indent--fuzzily
  '("def"
    "let"
    "with" "with/a"
    "fn" "fn/a")
  "Symbols that will have following lines indented +1 when matched at start.")

;;; Syntax Table

(defconst hy-mode-syntax-table
  (let ((table (copy-syntax-table lisp-mode-syntax-table)))
    (modify-syntax-entry ?\{ "(}" table)
    (modify-syntax-entry ?\} "){" table)
    (modify-syntax-entry ?\[ "(]" table)
    (modify-syntax-entry ?\] ")[" table)

    ;; Quote characters are prefixes
    (modify-syntax-entry ?\~ "'" table)
    (modify-syntax-entry ?\@ "'" table)

    ;; "," is a symbol in Hy, namely the tuple constructor
    (modify-syntax-entry ?\, "_" table)

    ;; "|" is a symbol in hy, naming the or operator
    (modify-syntax-entry ?\| "_" table)

    ;; "#" is a tag macro, we include # in the symbol
    (modify-syntax-entry ?\# "_" table)

    table)
  "Hy mode's syntax table.")

(defconst inferior-hy-mode-syntax-table (copy-syntax-table hy-mode-syntax-table)
  "`inferior-hy-mode' inherits `hy-mode-syntax-table'.")

;;; Indentation
;;;; Normal Indent

(defun hy-indent--normal (calculated-last-sexp-indent)
  "Get indent of the priorly let-bound `calculate-lisp-indent-last-sexp'

Example:
 (a (b c d
       e
       f))

1. Indent e => start at d (the last sexp) -> c -> b -> err.
=> backwards-sexp will throw error trying to jump to a
=> `hy-indent-function' returns nil
=> black magic then yields the correct indent

2. Indent f => start at e (the last sexp) -> loop done
=> backward-sexp loop terminates because the indentation caught up to the sexp
=> return indent of e

Users interested in the arcane (the nil case) can step through the part of
`calculate-lisp-indent' occurring right after `lisp-indent-function' is called.
Stepping through the trace is particularly useful in understanding indentation
commands."
  (goto-char calculated-last-sexp-indent)

  (let ((last-sexp-start))
    (cond ((ignore-errors
             (while (<= (current-indentation) (current-column))
               (setq last-sexp-start (prog1 (point) (backward-sexp))))
             t)
           (current-column))

          ((null last-sexp-start)
           (progn
             (when (-contains? '(?\' ?\` ?\~ ?\# ?\@) (char-before))
               (backward-char))
             (when (eq ?\~ (char-before))
               (backward-char))

             (1+ (current-column)))))))

;;;; Spec Finding

(defun hy-indent--syntax->spec (syntax)
  "Return integer for special indentation of form or nil to use normal indent."
  (-when-let (sym (and (hy--prior-sexp? syntax)
                       (thing-at-point 'symbol)))
    (or (-contains? hy-indent--exactly sym)
        (-some (-cut s-matches? <> sym) hy-indent--fuzzily))))

;;;; Hy indent function

(defun hy-indent-function (_indent-point syntax)
  "Given SYNTAX, the `parse-partial-sexp' corr. to INDENT-POINT, get indent."
  (hy--goto-inner-sexp syntax)

  (cond ((-contains? '(?\[ ?\{) (char-before))
         (current-column))

        ((hy-indent--syntax->spec syntax)
         (1+ (current-column)))

        (t (hy-indent--normal calculate-lisp-indent-last-sexp))))

;;; Bracket String Literals

(defconst hy--bracket-string-rx
  (rx "#["
      (0+ not-newline)
      "["
      (group (1+ (not (any "]"))))
      "]"
      (0+ not-newline)
      "]"))

(defun hy-syntax-propertize-function (start end)
  "Implements context sensitive syntax highlighting beyond `font-lock-keywords'.

In particular this implements bracket string literals.
START and END are the limits with which to search for bracket strings passed
and determined by `font-lock-mode' internals when making an edit to a buffer."
  (save-excursion
    (goto-char start)

    ;; Go to the start of the #[[ block
    (when (hy--goto-inner-char (syntax-ppss))
      (ignore-errors (backward-char 2)))

    (while (re-search-forward hy--bracket-string-rx end 'noerror)
      (-let ((a (match-beginning 1))
             (b (match-end 1))
             (string-fence (string-to-syntax "|")))
        (put-text-property (1- a) a 'syntax-table string-fence)
        (put-text-property b (1+ b) 'syntax-table string-fence)))))

;;; Shell Integration
;;;; Configuration

(defconst hy-shell-buffer-name "Hy"
  "Default buffer name for Hy interpreter.")

(defconst hy-shell-internal-buffer-name "Hy Internal"
  "Default buffer name for the internal Hy process.")

(defvar hy-shell-buffer nil
  "The current shell buffer for Hy.")

(defvar hy-shell-internal-buffer nil
  "The current internal shell buffer for Hy.")

(defvar hy--shell-output-filter-in-progress nil
  "Whether we are waiting for output in `hy-shell-send-string-no-output'.")

(defvar hy--shell-font-lock-enable t
  "Whether the shell should font-lock the current line.")

(defconst hy--shell-spy-delim-uuid "#cbb4fcbe-b6ba-4812-afa3-4a5ac7b20501"
  "UUID denoting end of python block in `--spy --control-categories' output")

;;;; Shell buffer utilities

(defun hy-installed? ()
  "Is the `hy-shell-interpreter' command available?"
  (when (executable-find hy-shell-interpreter) t))

(defun hy--shell-format-process-name (proc-name)
  "Format a PROC-NAME with closing astericks."
  (->> proc-name (s-prepend "*") (s-append "*")))

(defun hy-shell-get-process-name (&optional internal)
  "Get process name corr. to `hy-shell-buffer-name'/`hy-shell-internal-buffer-name'."
  (if internal
      hy-shell-internal-buffer-name
    hy-shell-buffer-name))

(defun hy-shell-get-process (&optional internal)
  "Get process corr. to `hy-shell-buffer-name'/`hy-shell-internal-buffer-name'."
  (get-process (hy-shell-get-process-name internal)))

(defun hy--shell-current-buffer-process ()
  "Get process associated with current buffer."
  (get-buffer-process (current-buffer)))

(defun hy--shell-current-buffer-a-process? ()
  "Is `current-buffer' a live process?"
  (process-live-p (hy--shell-current-buffer-process)))

(defun hy--shell-get-or-create-buffer ()
  "Get or create `hy-shell-buffer' buffer for current hy shell process."
  (if hy-shell-buffer
      hy-shell-buffer
    (hy--shell-with-shell-buffer
     (-let [proc-name
            (process-name (hy--shell-current-buffer-process))]
       (generate-new-buffer proc-name)))))

(defun hy--shell-buffer? (&optional internal)
  "Is `hy-shell-buffer'/`hy-shell-internal-buffer' set and does it exist?"
  (-let [buffer
         (if internal hy-shell-internal-buffer hy-shell-buffer)]
    (and buffer
         (buffer-live-p buffer))))

(defun hy--shell-kill-buffer (&optional internal)
  "Kill `hy-shell-buffer'/`hy-shell-internal-buffer'."
  (-let [buffer
         (if internal hy-shell-internal-buffer hy-shell-buffer)]
    (when (hy--shell-buffer? internal)
      (kill-buffer buffer)
      (when (derived-mode-p 'inferior-hy-mode)
        (setf buffer nil)))))

(defun hy-shell-kill ()
  "Kill all hy processes."
  (hy--shell-kill-buffer)
  (hy--shell-kill-buffer 'internal))

;;;; Shell macros

(defmacro hy--shell-with-shell-buffer (&rest forms)
  "Execute FORMS in the shell buffer."
  (-let [shell-process
         (gensym)]
    `(-let [,shell-process
            (hy-shell-get-process)]
       (with-current-buffer (process-buffer ,shell-process)
         ,@forms))))

(defmacro hy--shell-with-font-locked-shell-buffer (&rest forms)
  "Execute FORMS in the shell buffer with font-lock turned on."
  `(hy--shell-with-shell-buffer
    (save-current-buffer
      (unless (hy--shell-buffer?)
        (setq hy-shell-buffer (hy--shell-get-or-create-buffer)))
      (set-buffer hy-shell-buffer)

      (unless (font-lock-mode) (font-lock-mode 1))
      (unless (derived-mode-p 'hy-mode) (hy-mode))

      ,@forms)))

;;;; Font locking

(defun hy--shell-faces-to-font-lock-faces (text &optional start-pos)
  "Set all 'face in TEXT to 'font-lock-face optionally starting at START-POS."
  (let ((pos 0)
        (start-pos (or start-pos 0)))
    (while (and (/= pos (length text))
                (setq next (next-single-property-change pos 'face text)))
      (-let* ((plist (text-properties-at pos text))
              ((&plist 'face face) plist))
        (set-text-properties (+ start-pos pos) (+ start-pos next)
                             (-doto plist
                               (plist-put 'face nil)
                               (plist-put 'font-lock-face face)))
        (setq pos next)))))

(defun hy--shell-fontify-prompt-post-command-hook ()
  "Fontify just the current line in `hy-shell-buffer' for `post-command-hook'.

Constantly extracts current prompt text and executes and manages applying
`hy--shell-faces-to-font-lock-faces' to the text."
  (-when-let* (((_ . prompt-end) comint-last-prompt)
               (_ (and prompt-end
                       (> (point) prompt-end)  ; new command is being entered
                       (hy--shell-current-buffer-a-process?))))  ; process alive?
    (let* ((input (buffer-substring-no-properties prompt-end (point-max)))
           (deactivate-mark nil)
           (buffer-undo-list t)
           (font-lock-buffer-pos nil)
           (text (hy--shell-with-font-locked-shell-buffer
                  (delete-region (line-beginning-position) (point-max))
                  (setq font-lock-buffer-pos (point))
                  (insert input)
                  (font-lock-ensure)
                  (buffer-substring font-lock-buffer-pos (point-max)))))
      (hy--shell-faces-to-font-lock-faces text prompt-end))))

(defun hy--shell-font-lock-turn-on ()
  "Turn on fontification of current line for hy shell."
  (hy--shell-with-shell-buffer
   (hy--shell-kill-buffer)

   ;; Required - I don't understand why killing doesn't capture the end nil setq
   (setq-local hy-shell-buffer nil)

   (add-hook 'post-command-hook
             'hy--shell-fontify-prompt-post-command-hook nil 'local)
   (add-hook 'kill-buffer-hook
             'hy--shell-kill-buffer nil 'local)))

(defun hy--shell-font-lock-spy-output (string)
  "Applies font-locking to hy outputted python blocks when `--spy' is enabled."
  (with-temp-buffer
    (if (s-contains? hy--shell-spy-delim-uuid string)
        (-let ((python-indent-guess-indent-offset
                nil)
               ((python-block hy-output)
                (s-split hy--shell-spy-delim-uuid string)))
          (python-mode)
          (insert python-block)
          (font-lock-default-fontify-buffer)
          (-> (buffer-string)
              hy--shell-faces-to-font-lock-faces
              s-chomp
              (s-concat hy-shell-spy-delim hy-output)))
      string)))

;;;; Send strings

(defun hy--shell-end-of-output? (string)
  "Return non-nil if STRING ends with the prompt."
  (s-matches? comint-prompt-regexp string))

(defun hy--shell-output-filter (string)
  "If STRING ends with input prompt then set filter in progress done."
  (when (hy--shell-end-of-output? string)
    (setq hy--shell-output-filter-in-progress nil))
  "\n=> ")

(defun hy--shell-send-string (string &optional process internal)
  "Internal implementation of shell send string functionality."
  (-let ((process (or process
                      (hy-shell-get-process internal)))
         (hy--shell-output-filter-in-progress t))
    (unless process
      (error "No active Hy process found/given!"))
    (comint-send-string process string)
    (when (or (not (string-match "\n\\'" string))
              (string-match "\n[ \t].*\n?\\'" string))
      (comint-send-string process "\n"))))

(defun hy-shell-send-string-no-output (string &optional process internal)
  "Send STRING to hy PROCESS and inhibit printing output."
  (let* ((comint-preoutput-filter-functions
          '(hy--shell-output-filter))
         (process (or process (hy-shell-get-process internal)))
         (hy--shell-output-filter-in-progress t)
         (inhibit-quit t))
    (unless process
      (error "No active Hy process found/given!"))
    (or (with-local-quit
          (hy--shell-send-string string process internal)
          (while hy--shell-output-filter-in-progress
            (accept-process-output process))
          t)
        (with-current-buffer (process-buffer process)
          (comint-interrupt-subjob)))))

(defun hy-shell-send-string-internal (string)
  "Send STRING to internal hy shell process."
  (hy-shell-send-string-no-output string nil 'internal))

(defun hy--shell-send-internal-setup-code ()
  "Send setup code for autocompletion and eldoc to hy internal process."
  (hy-shell-send-string-internal (concat hy-eldoc-setup-code
                                         hy-company-setup-code)))

(defun hy-shell-send-string (string &optional process)
  "Send STRING to hy PROCESS."
  (-let [comint-output-filter-functions
         '(hy--shell-output-filter)]
    (hy--shell-send-string string process)))

(defun hy--shell-send-async (string)
  "Send STRING to internal hy process asynchronously."
  (let ((output-buffer " *Comint Hy Redirect Work Buffer*")
        (proc (hy-shell-get-process 'internal)))
    (with-current-buffer (get-buffer-create output-buffer)
      (erase-buffer)
      (comint-redirect-send-command-to-process string output-buffer proc nil t)

      (set-buffer (process-buffer proc))
      (while (and (null comint-redirect-completed)
                  (accept-process-output proc nil 100 t)))
      (set-buffer output-buffer)
      (buffer-string))))

;;;; Update internal process

(defun hy--shell-update-imports ()
  "Send imports/requires to the current internal process.

This is currently done manually as I'm not sure of the consequences of doing
so automatically through eg. regular intervals. Sending the imports allows
Eldoc to function (and will allow autocompletion once modules are completed).

Right now the keybinding is not publically exposed."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward (rx "(" (0+ space)
                                  (or "import" "require" "sys.path.extend"))
                              nil t)
      (hy-shell-send-string-internal (hy--current-form-string)))))

;;;; Shell creation

(defun hy--shell-calculate-interpreter-args ()
  "Calculate `hy-shell-interpreter-args' based on `--spy' flag."
  (if (and hy-shell-use-control-codes?
           (s-contains? "--spy" hy-shell-interpreter-args))
      (s-concat hy-shell-interpreter-args " --control-codes")
    hy-shell-interpreter-args))

(defun hy--shell-calculate-command (&optional internal)
  "Calculate the string used to execute the inferior Hy process."
  (format "%s %s"
          (shell-quote-argument hy-shell-interpreter)
          (if internal
              ""
            (hy--shell-calculate-interpreter-args))))

(defun hy--shell-make-comint (cmd proc-name &optional show internal)
  "Create and return comint process PROC-NAME with CMD, opt. INTERNAL and SHOW."
  (-when-let* ((proc-buffer-name
                (hy--shell-format-process-name proc-name))
               (_
                (not (comint-check-proc proc-buffer-name)))
               (cmdlist
                (split-string-and-unquote cmd))
               (buffer
                (apply 'make-comint-in-buffer proc-name proc-buffer-name
                       (car cmdlist) nil (cdr cmdlist)))
               (process
                (get-buffer-process buffer)))
    (with-current-buffer buffer
      (unless (derived-mode-p 'inferior-hy-mode)
        (inferior-hy-mode)))
    (when show
      (display-buffer buffer))
    (if internal
        (progn
          (set-process-query-on-exit-flag process nil)
          (setq hy-shell-internal-buffer buffer))
      (setq hy-shell-buffer buffer))
    proc-buffer-name))

;;;; Run Shell

(defun run-hy-internal ()
  "Start an inferior hy process in the background for autocompletion."
  (interactive)
  (unless (hy-installed?)
    (message "Hy not found, activate a virtual environment containing Hy to use
Eldoc, Anaconda, and other hy-mode features."))

  (when (and (not (hy-shell-get-process 'internal))
             (hy-installed?))
    (-let [hy--shell-font-lock-enable
           nil]
      (prog1
          (-> (hy--shell-calculate-command 'internal)
              (hy--shell-make-comint (hy-shell-get-process-name 'internal) nil 'internal)
              get-buffer-process)
        (hy--shell-send-internal-setup-code)
        (message "Hy internal process successfully started")))))

(defun run-hy (&optional cmd)
  "Run an inferior Hy process.

CMD defaults to the result of `hy--shell-calculate-command'."
  (interactive)
  (unless (hy-installed?)
    (message "Hy not found, activate a virtual environment with Hy."))

  (-> (or cmd (hy--shell-calculate-command))
      (hy--shell-make-comint hy-shell-buffer-name 'show)
      get-buffer-process))

;;; Eldoc
;;;; Setup Code

(defconst hy-eldoc-setup-code
  "(import builtins)
(import inspect)

(defn --HYDOC-format-argspec [argspec]
  \"Lispy version of format argspec covering all defun kwords.\"
  (setv docs \"\")

  (defmacro add-docs [&rest forms]
    `(do (when docs (setv docs (+ docs \" \")))
         (setv docs (+ docs ~@forms))))

  (defn format-args [&rest args]
    (->> args
       (map (fn [x] (-> x str (.replace \"_\" \"-\"))))
       (.join \" \")))

  (setv args argspec.args)
  (setv defaults argspec.defaults)
  (setv varargs argspec.varargs)
  (setv varkw argspec.varkw)
  (setv kwonlyargs argspec.kwonlyargs)
  (setv kwonlydefaults argspec.kwonlydefaults)

  (when (and args defaults)
    (setv args (-> argspec.defaults
                  len
                  (drop-last argspec.args)
                  list))
    (setv defaults (-> args
                      len
                      (drop argspec.args)
                      list)))

  (when (and kwonlyargs kwonlydefaults)
    (setv kwonlyargs (->> kwonlyargs
                        (remove (fn [x] (in x (.keys kwonlydefaults))))
                        list))
    (setv kwonlydefaults (->> kwonlydefaults
                            (.items)
                            (*map (fn [k v] (.format \"[{} {}]\" k v)))
                            list)))

  (when args
    (add-docs (format-args #* args)))
  (when defaults
    (add-docs \"&optional \"
              (format-args #* defaults)))
  (when varargs
    (add-docs \"&rest \"
              (format-args varargs)))
  (when varkw
    (add-docs \"&kwargs \"
              (format-args varkw)))
  (when kwonlyargs
    (add-docs \"&kwonly \"
              (format-args #* kwonlyargs)))
  (when kwonlydefaults
    (add-docs (if-not kwonlyargs \"&kwonly \" \"\")
              (format-args #* kwonlydefaults)))

  docs)

(defn --HYDOC-format-eldoc-string [obj-name f &optional full]
  \"Format an obj name for callable f.\"
  (if f.--doc--
      (.format \"{obj}: ({args}){docs}\"
               :obj (.replace obj-name \"_\" \"-\")
               :args (--HYDOC-format-argspec (inspect.getfullargspec f))
               :docs (if full
                         (->> f.--doc-- (.splitlines) (.join \"\n\") (+ \"\n\"))
                         (+ \" - \" (->> f.--doc-- (.splitlines) first))))
      (.format \"{obj}: ({args})\"
               :obj (.replace obj-name \"_\" \"-\")
               :args (--HYDOC-format-argspec (inspect.getfullargspec f)))))

(defn --HYDOC-python-eldoc [obj &optional full]
  \"Build eldoc string for python obj or string.

Not all defuns can be argspeced - eg. C defuns.\"
  (try
    (do (when (isinstance obj str)
          (setv obj (.eval builtins obj (globals))))
        (setv full-doc (.getdoc inspect obj))
        (setv doc full-doc)
        (try
          (setv doc (--HYDOC-format-eldoc-string obj.--name-- obj
                                                :full full))
          (except [e TypeError]
            (setv doc (->> doc (.splitlines) first (+ \"builtin: \")))
            (when full
              (setv doc (+ doc \"\n\"
                           (->> full-doc (.splitlines) rest (.join \"\"))))))))
    (except [e Exception]
      (setv doc \"\")))
  doc)

(defn --HYDOC-macro-eldoc [obj &optional full]
  \"Get eldoc string for a macro.\"
  (try
    (do (setv obj (.replace obj \"-\" \"_\"))
        (setv macros (get --macros-- None))

        (when (in obj macros)
          (--HYDOC-format-eldoc-string obj (get macros obj) :full full)))
    (except [e Exception] \"\")))

(defn --HYDOC [obj &optional full]
  \"Get eldoc string for any obj.\"
  (setv doc (--HYDOC-python-eldoc obj :full full))
  (unless doc (setv doc (--HYDOC-macro-eldoc obj :full full)))
  doc)"
  "Symbol introspection code to send to the internal process for eldoc.")

;;;; Utilities

(defun hy--eldoc-chomp-output (text)
  "Chomp prefixes and suffixes from eldoc process output."
  (->> text
       (s-chop-suffixes '("\n=> " "=> " "\n"))
       (s-chop-prefixes '("\"" "'" "\"'" "'\""))
       (s-chop-suffixes '("\"" "'" "\"'" "'\""))))

(defun hy--eldoc-remove-syntax-errors (text)
  "Quick fix to address parsing an incomplete dot-dsl."
  (if (< 1 (-> text s-lines length))
      ""
    text))

(defun hy--eldoc-send (string)
  "Send STRING for eldoc to internal process returning output."
  (-> string
      hy--shell-send-async
      hy--eldoc-chomp-output
      hy--eldoc-remove-syntax-errors
      hy--str-or-nil))

(defun hy--eldoc-format-command (symbol &optional full raw)
  "Inspect SYMBOL with hydoc, optionally include FULL docs for a buffer."
  (format "(try (--HYDOC %s :full %s) (except [e Exception] (str)))"
          (if raw symbol (s-concat "\"" symbol "\""))
          (if full "True" "False")))

(defun hy--not-function-form? ()
  "Non-nil if form at point doesn't represent a function call."
  (or (-contains? '(?\[ ?\{) (char-after))
      (not (looking-at (rx anything  ; Skips form opener
                           (or (syntax symbol) (syntax word)))))))


(defun hy--eldoc-get-inner-symbol ()
  "Traverse and inspect innermost sexp and return formatted string for eldoc."
  (save-excursion
    (-when-let (function
                (and (hy-shell-get-process 'internal)
                     (-some-> (syntax-ppss) hy--sexp-inermost-char goto-char)
                     (not (hy--not-function-form?))
                     (progn (forward-char) (thing-at-point 'symbol))))

      ;; Attribute method call (eg. ".format str") needs following sexp
      (if (and (s-starts-with? "." function)
               (ignore-errors (forward-sexp) (forward-char) t))
          (pcase (char-after)
            ;; Can't send just .method to eldoc
            ((or ?\) ?\s ?\C-j) nil)

            ;; Dot dsl doesn't work on literals
            (?\[ (concat "list" function))
            (?\{ (concat "dict" function))
            (?\" (concat "str" function))

            ;; Otherwise complete the dot dsl
            (_ (progn
                 (forward-char)
                 (concat (thing-at-point 'symbol) function))))
        function))))

(defun hy--fontify-text (text regexp &rest faces)
  "Fontify portions of TEXT matching REGEXP with FACES."
  (when text
    (-each
        (s-matched-positions-all regexp text)
      (-lambda ((beg . end))
        (--each faces
          (add-face-text-property beg end it nil text))))))

(defun hy--eldoc-fontify-text (text)
  "Fontify eldoc TEXT."
  (let ((kwd-rx
         (rx string-start (1+ (not (any space ":"))) ":"))
        (kwargs-rx
         (rx symbol-start "&" (1+ word)))
        (quoted-args-rx
         (rx "`" (1+ (not space)) "`")))
    (hy--fontify-text
     text kwd-rx 'font-lock-keyword-face)
    (hy--fontify-text
     text kwargs-rx 'font-lock-type-face)
    (hy--fontify-text
     text quoted-args-rx 'font-lock-constant-face 'bold-italic))
  text)

;;;; Documentation Functions

(defun hy--eldoc-get-docs (obj &optional full)
  "Get eldoc or optionally buffer-formatted docs for `obj'."
  (when obj
    (hy--eldoc-fontify-text
     (or (-> obj (hy--eldoc-format-command full) hy--eldoc-send)
         (-> obj (hy--eldoc-format-command full t) hy--eldoc-send)))))

(defun hy-eldoc-documentation-function ()
  "Drives `eldoc-mode', retrieves eldoc msg string for inner-most symbol."
  (-> (hy--eldoc-get-inner-symbol)
      hy--eldoc-get-docs))

;;; Describe thing at point

(defun hy--docs-for-thing-at-point ()
  "Mirrors `hy-eldoc-documentation-function' formatted for a buffer, not a msg."
  (-> (thing-at-point 'symbol)
      (hy--eldoc-get-docs t)
      hy--format-docs-for-buffer))

(defun hy--format-docs-for-buffer (text)
  "Format raw hydoc TEXT for inserting into hyconda buffer."
  (-let [kwarg-newline-regexp
         (rx ","
             (1+ (not (any "," ")")))
             (group-n 1 "\\\n")
             (1+ (not (any "," ")"))))]
    (-some--> text
              (s-replace "\\n" "\n" it)
              (replace-regexp-in-string kwarg-newline-regexp
                                        "newline" it nil t 1))))

(defun hy-describe-thing-at-point ()
  "Implement shift-k docs lookup for `spacemacs/evil-smart-doc-lookup'."
  (interactive)
  (-when-let* ((text (hy--docs-for-thing-at-point))
               (doc-buffer "*Hyconda*"))
    (with-current-buffer (get-buffer-create doc-buffer)
      (erase-buffer)
      (switch-to-buffer-other-window doc-buffer)

      (insert text)
      (goto-char (point-min))
      (forward-line)

      (insert "------\n")
      (fill-region (point) (point-max))

      ;; Eventually make hyconda-view-minor-mode, atm this is sufficient
      (local-set-key "q" 'quit-window)
      (when (fboundp 'evil-local-set-key)
        (evil-local-set-key 'normal "q" 'quit-window)))))

;;; Autocompletion

(defconst hy-company-setup-code
  "(import builtins)
(import hy)
(try
 (import [hy.lex.parser [hy-symbol-unmangle hy-symbol-mangle]])
 (except [e ImportError]
         (import [hy.lex [unmangle :as hy-symbol-unmangle
                          mangle :as hy-symbol-mangle]])))
(try
 (import [hy.compiler [-compile-table]])
 (except [e ImportError]
         (import [hy.compiler [-special-form-compilers :as -compile-table]])))

(import [hy.core.shadow [*]])
(import [hy.core.language [*]])

(defn --HYCOMPANY-get-obj [text]
  (when (in \".\" text)
    (.join \".\" (-> text (.split \".\") butlast))))

(defn --HYCOMPANY-get-attr [text]
  (if (in \".\" text)
      (-> text (.split \".\") last)
      text))

(defn --HYCOMPANY-get-obj-candidates [obj]
  (try
    (->> obj builtins.eval dir (map hy-symbol-unmangle) list)
    (except [e Exception]
      [])))

(defn --HYCOMPANY-get-macros []
  \"Extract macro names from all namespaces and compile-table symbols.\"
  (->> --macros--
     (.keys)
     (chain (.keys -compile-table))
     flatten
     (map --HYCOMPANY-get-name)
     (map hy-symbol-unmangle)
     distinct
     list))

(defn --HYCOMPANY-get-global-candidates []
  (->> (globals)
     (.keys)
     (map hy-symbol-unmangle)
     (chain (--HYCOMPANY-get-macros))
     flatten
     distinct
     list))

(defn --HYCOMPANY-get-name [x]
  \"Return the candidate name for x.\"
  (if (isinstance x str)
      x
      x.--name--))

(defn --HYCOMPANY-trim-candidates [candidates attr]
  \"Limit list of candidates to those starting with attr.\"
  (list (filter (fn [cand] (.startswith cand attr)) candidates)))

(defn --HYCOMPANY [text]
  (setv obj (--HYCOMPANY-get-obj text))
  (setv attr (--HYCOMPANY-get-attr text))

  (if obj
      (setv candidates (--HYCOMPANY-get-obj-candidates obj))
      (setv candidates (--HYCOMPANY-get-global-candidates)))

  (setv choices (--HYCOMPANY-trim-candidates candidates attr))

  (if obj
      (list (map (fn [x] (+ obj \".\" x))
                 choices))
      choices))

(defn --HYANNOTATE-search-builtins [text]
  (setv text (hy-symbol-mangle text))
  (try
    (do (setv obj (builtins.eval text))
        (setv obj-name obj.--class--.--name--)
        (cond [(in obj-name [\"function\" \"builtin_function_or_method\"])
               \"def\"]
              [(= obj-name \"type\")
               \"class\"]
              [(= obj-name \"module\")
               \"module\"]
              [True \"instance\"]))
    (except [e Exception]
      None)))

(defn --HYANNOTATE-search-compiler [text]
  (in text -compile-table))

(defn --HYANNOTATE-search-shadows [text]
  (->> hy.core.shadow
     dir
     (map hy-symbol-unmangle)
     (in text)))

(defn --HYANNOTATE-search-macros [text]
  (setv text (hy-symbol-mangle text))
  (.get --macros-- text None))

(defn --HYANNOTATE [x]
  ;; only builtins format on case basis
  (setv annotation (--HYANNOTATE-search-builtins x))
  (when (and (not annotation) (--HYANNOTATE-search-shadows x))
    (setv annotation \"shadowed\"))
  (when (and (not annotation) (--HYANNOTATE-search-compiler x))
    (setv annotation \"compiler\"))
  (when (and (not annotation) (--HYANNOTATE-search-macros x))
    (setv annotation \"macro\"))

  (if annotation
      (.format \"<{} {}>\" annotation x)
      \"\"))"
  "Autocompletion setup code to send to the internal process.")

(defconst hy--company-regexp
  (rx "'"
      (group (1+ (not (any ",]"))))
      "'"
      (any "," "]"))
  "Regex to extra candidates from --HYCOMPANY.")

(defun hy--company-format-str (string)
  "Format STRING to send to hy for completion candidates."
  (-some->> string (format "(--HYCOMPANY \"%s\")" )))

(defun hy--company-format-annotate-str (string)
  "Format STRING to send to hy for its annotation."
  (-some->> string (format "(--HYANNOTATE \"%s\")")))

(defun hy--company-annotate (candidate)
  "Get company annotation for CANDIDATE string."
  (-some->> candidate
            hy--company-format-annotate-str
            hy--shell-send-async
            s-chomp
            (s-chop-prefix "'")
            (s-chop-suffix "'")))

(defun hy--company-candidates (string)
  "Get candidates for completion of STRING."
  (unless (s-starts-with? "." string)
    (-some->> string
              hy--company-format-str
              hy--shell-send-async
              (s-match-strings-all hy--company-regexp)
              (-select-column 1))))

(defun company-hy (command &optional arg &rest ignored)
  (interactive (list 'interactive))
  (cl-case command
    (prefix (company-grab-symbol))
    (candidates (hy--company-candidates arg))
    (annotation (hy--company-annotate arg))
    (meta (-> arg hy--eldoc-get-docs hy--str-or-empty))))

;;; Keybindings

;;;###autoload
(defun hy-insert-pdb ()
  "Import and set pdb trace at point."
  (interactive)
  (insert "(do (import pdb) (pdb.set-trace))"))

;;;###autoload
(defun hy-insert-pdb-threaded ()
  "Import and set pdb trace at point for a threading macro."
  (interactive)
  (insert "((fn [x] (import pdb) (pdb.set-trace) x))"))

;;;###autoload
(defun hy-shell-start-or-switch-to-shell ()
  (interactive)
  (if (and (hy--shell-buffer?) (get-buffer-process hy-shell-buffer))
      (switch-to-buffer-other-window
       (hy--shell-get-or-create-buffer))
    (run-hy)))

;;;###autoload
(defun hy-shell-eval-buffer ()
  "Send the buffer to the shell, inhibiting output."
  (interactive)
  (-let [text
         (buffer-string)]
    (unless (hy--shell-buffer?)
      (hy-shell-start-or-switch-to-shell))
    (hy--shell-with-shell-buffer
     (hy-shell-send-string-no-output text))))

;;;###autoload
(defun hy-shell-eval-region ()
  "Send highlighted region to shell, inhibiting output."
  (interactive)
  (when (and (region-active-p)
             (not (region-noncontiguous-p)))
    (-let [text
           (buffer-substring (region-beginning) (region-end))]
      (unless (hy--shell-buffer?)
        (hy-shell-start-or-switch-to-shell))
      (hy--shell-with-shell-buffer
       (hy-shell-send-string-no-output text)))))

;;;###autoload
(defun hy-shell-eval-current-form ()
  "Send form containing current point to shell."
  (interactive)
  (-when-let (text (hy--current-form-string))
    (unless (hy--shell-buffer?)
      (hy-shell-start-or-switch-to-shell))
    (hy--shell-with-shell-buffer
     (hy-shell-send-string text))))

;;;###autoload
(defun hy-shell-eval-last-sexp ()
  "Send form containing the last s-expression to shell."
  (interactive)
  (-when-let (text (hy--last-sexp-string))
    (unless (hy--shell-buffer?)
      (hy-shell-start-or-switch-to-shell))
    (hy--shell-with-shell-buffer
     (hy-shell-send-string text))))

;;; hy-mode and inferior-hy-mode
;;;; Hy-mode setup

(defun hy--mode-setup-eldoc ()
  (make-local-variable 'eldoc-documentation-function)
  (setq-local eldoc-documentation-function 'hy-eldoc-documentation-function)
  (eldoc-mode +1))

(defun hy--mode-setup-font-lock ()
  (setq-local font-lock-multiline t)
  (setq font-lock-defaults
        '(hy-font-lock-kwds
          nil nil
          (("+-*/.<>=!?$%_&~^:@" . "w"))  ; syntax alist
          nil
          (font-lock-mark-block-function . mark-defun)
          (font-lock-syntactic-face-function  ; Differentiates (doc)strings
           . hy-font-lock-syntactic-face-function))))

(defun hy--mode-setup-inferior ()
  ;; (add-to-list 'company-backends 'company-hy)
  (setenv "PYTHONIOENCODING" "UTF-8")

  (run-hy-internal)
  (add-hook 'pyvenv-post-activate-hooks 'run-hy-internal nil t))

(defun hy--mode-setup-syntax ()
  ;; We explictly set it for tests that only call this setup-fn
  (set-syntax-table hy-mode-syntax-table)

  ;; Bracket string literals require context sensitive highlighting
  (setq-local syntax-propertize-function 'hy-syntax-propertize-function)

  ;; AutoHighlightSymbol needs adjustment for symbol recognition
  (setq-local ahs-include "^[0-9A-Za-z/_.,:;*+=&%|$#@!^?-~\-]+$")

  ;; Lispy comment syntax
  (setq-local comment-start ";")
  (setq-local comment-start-skip
              "\\(\\(^\\|[^\\\\\n]\\)\\(\\\\\\\\\\)*\\)\\(;+\\|#|\\) *")
  (setq-local comment-add 1)

  ;; Lispy indent with hy-specialized indentation
  (setq-local indent-tabs-mode nil)
  (setq-local indent-line-function 'lisp-indent-line)
  (setq-local lisp-indent-function 'hy-indent-function))

(defun hy--mode-setup-smartparens ()
  "Setup smartparens, if active, pairs for Hy."
  (when (fboundp 'sp-local-pair)
    (sp-local-pair '(hy-mode) "`" "`" :actions nil)
    (sp-local-pair '(hy-mode) "'" "'" :actions nil)
    (sp-local-pair '(inferior-hy-mode) "`" "" :actions nil)
    (sp-local-pair '(inferior-hy-mode) "'" "" :actions nil)))

;;;; Inferior-hy-mode setup

(defun hy--inferior-mode-setup ()
  ;; Comint config
  (setq mode-line-process '(":%s"))
  (setq-local indent-tabs-mode nil)
  (setq-local comint-prompt-read-only t)
  (setq-local comint-prompt-regexp (rx bol "=>" space))

  ;; Highlight errors according to colorama python package
  (ansi-color-for-comint-mode-on)
  (setq-local comint-output-filter-functions '(ansi-color-process-output))

  ;; Don't startup font lock for internal processes
  (when hy--shell-font-lock-enable
    (if (fboundp 'xterm-color-filter)
        (setq-local comint-preoutput-filter-functions
                    `(xterm-color-filter hy--shell-font-lock-spy-output))
      (setq-local comint-preoutput-filter-functions
                  `(hy--shell-font-lock-spy-output)))
    (hy--shell-font-lock-turn-on))

  ;; Fixes issue with "=>", no side effects from this advice
  (advice-add 'comint-previous-input :before
              (lambda (&rest args) (setq-local comint-stored-incomplete-input ""))))

;;; Core

(add-to-list 'auto-mode-alist '("\\.hy\\'" . hy-mode))
(add-to-list 'interpreter-mode-alist '("hy" . hy-mode))

;;;###autoload
(define-derived-mode inferior-hy-mode comint-mode "Inferior Hy"
  "Major mode for Hy inferior process."
  (hy--inferior-mode-setup))

;;;###autoload
(define-derived-mode hy-mode prog-mode "Hy"
  "Major mode for editing Hy files."
  (hy--mode-setup-eldoc)
  (hy--mode-setup-font-lock)
  (when hy-shell-internal?
    (hy--mode-setup-inferior))
  (hy--mode-setup-smartparens)
  (hy--mode-setup-syntax))

;; Spacemacs users please see spacemacs-hy, all bindings defined there
(set-keymap-parent hy-mode-map lisp-mode-shared-map)
(define-key hy-mode-map (kbd "C-c C-z") 'hy-shell-start-or-switch-to-shell)
(define-key hy-mode-map (kbd "C-c C-b") 'hy-shell-eval-buffer)
(define-key hy-mode-map (kbd "C-c C-t") 'hy-insert-pdb)
(define-key hy-mode-map (kbd "C-c C-S-t") 'hy-insert-pdb-threaded)
(define-key hy-mode-map (kbd "C-c C-r") 'hy-shell-eval-region)
(define-key hy-mode-map (kbd "C-M-x") 'hy-shell-eval-current-form)
(define-key hy-mode-map (kbd "C-c C-e") 'hy-shell-eval-last-sexp)
(define-key inferior-hy-mode-map (kbd "C-c C-z") (lambda ()
                                                   (interactive)
                                                   (other-window -1)))

(provide 'hy-mode)

;;; hy-mode.el ends here
