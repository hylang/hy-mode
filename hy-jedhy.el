;;; hy-jedhy.el --- Jedhy integration -*- lexical-binding: t -*-

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

;; Interface with the Hy pkg `jedhy' for IDE components.

;; Implements the following functions (follows standard naming conventions):
;; - `company-hy'
;; - `hy-eldoc-documentation-function'
;; - `hy-describe-thing-at-point'
;; - `hy-jedhy-update-imports'

;;; Code:

(require 'hy-base)

(require 'hy-shell)

;;; Configuration
;;;; Configured

(defvar hy-jedhy--enable? t
  "Should an internal process startup for use by ide components?")

;;;; Managed

(defvar-local hy-jedhy--running? nil
  "Was `jedhy' successfully started up in the current buffer?")

(defvar hy-jedhy--doc-lookup-buffer " *Hy Doc Lookup Buffer"
  "The buffer name to use for documentation lookups.")

;;;; Hy Code

;; TODO Code is one-line until I figure out how to concatenate redirected output
;; TODO Would like to auto-update namespace, but not sure how
;; TODO Would prefer to use the .set-namespace instead of remaking jedhy instance

(defconst hy-jedhy--setup-code
  "(import hy [hy.core.language [*]] [hy.core.macros [*]]) (require [hy.extra.anaphoric [*]]) (try (do (import jedhy jedhy.api) (setv --JEDHY (jedhy.api.API)) \"Started jedhy\") (except [e Exception] \"Failed to start jedhy\"))"
  "Text to send to internal Hy process to setup `jedhy'.")

(defconst hy-jedhy--startup-success-text "'Started jedhy'"
  "Text identifying successful startup of jedhy.")

(defconst hy-jedhy--reset-namespace-code
  "(setv --JEDHY (jedhy.api.API :locals- (locals) :globals- (globals) :macros- --macros--))"
  "Text to send to make Jedhy's namespace current.")

;;; Startup

(defun hy-jedhy--startup ()
  "Startup jedhy and notify its status, returning non-nil if successful."
  (hy-shell--with-internal
    (if hy-jedhy--running?
        (hy-shell--notify "Jedhy should already be running")
      (if (s-equals? hy-jedhy--startup-success-text
                     (hy-shell--redirect-send-internal hy-jedhy--setup-code))
          (prog1 t
            (setq-local hy-jedhy--running? t)
            (hy-shell--notify "Jedhy successfully started"))
        (hy-shell--notify "Jedhy failed to start")))))

;;; Namespace Management

(defconst hy-shell--import-rgx
  (rx "(" (0+ space) (or "import" "require" "sys.path.extend"))
  "A regex used to extract importing-related forms for updating IDE features.")

(defun hy-jedhy-update-imports ()
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
      (-when-let (hy-form (hy--current-form-string))
        (let ((text (s-join " " (s-lines hy-form))))
          (hy-shell--redirect-send-internal text))))

    (hy-shell--redirect-send-internal hy-jedhy--reset-namespace-code)))

;;; Dot DSL Completion

(defun hy-jedhy--method-call? (symbol)
  "Is SYMBOL a method call in Hy?"
  (s-starts-with? "." symbol))

(defun hy-jedhy--quickfix-eldoc-dot-dsl-syntax-errors (text)
  "Quick fix to address parsing an incomplete dot-dsl."
  (if (< 1 (-> text s-lines length))
      ""
    text))

(defun hy-jedhy--get-inner-symbol ()
  "Get inner symbol for point, completing Hy's method-dot DSL if applicable."
  (save-excursion
    (-when-let (inner-symbol (and (hy--goto-inner-sexp (syntax-ppss))
                                  (not (-contains? '(?\[ ?\{) (char-before)))
                                  (thing-at-point 'symbol)))
      (if (hy-jedhy--method-call? inner-symbol)
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

;;; Output Handling
;;;; Formatting

(defun hy-jedhy--format-output-str (output)
  "Format OUTPUT given as a string."
  (->> output
     (s-chop-prefixes '("'" "\""))
     (s-chop-suffixes '("'" "\""))))

(defun hy-jedhy--format-output-tuple (output)
  "Format OUTPUT given as a tuple."
  (unless (s-equals? "()" output)
    (->> output
       (s-replace-all '(("'" . "")
                        (",)" . "")  ; one element list case
                        ("(" . "")
                        (")" . "")))
       (s-split ", "))))  ; comma is a valid token so can't replace it

(defun hy-jedhy--format-describe-output (output)
  "Converts escaped newlines to true newlines."
  (let ((kwarg-newline-regexp (rx ","
                                  (1+ (not (any "," ")")))
                                  (group-n 1 "\\\n")
                                  (1+ (not (any "," ")"))))))
    (-some-->
     output
     (s-replace "\\n" "\n" it)
     (replace-regexp-in-string kwarg-newline-regexp "newline" it nil t 1))))

;;;; Fontifying

(defun hy-jedhy--fontify-text (text regexp &rest faces)
  "Fontify portions of TEXT matching REGEXP with FACES."
  (when text
    (-each (s-matched-positions-all regexp text)
      (-lambda ((start . end))
        (-each faces
          (lambda (face)
            (add-face-text-property start end face nil text)))))))

(defun hy-jedhy--fontify-eldoc (text)
  "Fontify eldoc TEXT."
  (let ((kwd-rx
         (rx string-start (1+ (not (any space ":"))) ":"))
        (unpack-rx
         (rx (or "#*" "#**")))
        (kwargs-rx
         (rx symbol-start "&" (1+ word)))
        (quoted-rx
         (rx "`" (1+ (not space)) "`")))
    (hy-jedhy--fontify-text text kwd-rx 'font-lock-keyword-face)
    (hy-jedhy--fontify-text text unpack-rx 'font-lock-keyword-face)
    (hy-jedhy--fontify-text text kwargs-rx 'font-lock-type-face)
    (hy-jedhy--fontify-text text quoted-rx 'font-lock-constant-face 'bold-italic))
  text)

(defun hy-jedhy--fontify-first-docs-line (output)
  "Fontify only the first line of jedhy OUTPUT accordding to eldoc."
  (when output
    (-let (((leader . rest) (s-lines output)))
      (s-join "\n"
              (cons (hy-jedhy--fontify-eldoc leader)
                    rest)))))

;;; Jedhy Interface

(defun hy-jedhy--prefix-str->candidates (prefix-str)
  "Get company candidates for a PREFIX-STR."
  (unless (hy-jedhy--method-call? prefix-str)
    (-some->>
     prefix-str
     (format "(--JEDHY.complete \"%s\")")
     hy-shell--redirect-send-internal
     hy-jedhy--format-output-tuple)))

(defun hy-jedhy--candidate-str->annotation (candidate-str)
  "Get company annotation for a CANDIDATE-STR."
  (-some->>
   candidate-str
   (format "(--JEDHY.annotate \"%s\")")
   hy-shell--redirect-send-internal
   hy-jedhy--format-output-str))

(defun hy-jedhy--candidate-str->eldoc (candidate-str)
  "Get eldoc docstring for a CANDIDATE-STR."
  (-some->>
   candidate-str
   (format "(--JEDHY.docs \"%s\")")
   hy-shell--redirect-send-internal
   hy-jedhy--format-output-str
   hy-jedhy--quickfix-eldoc-dot-dsl-syntax-errors
   hy-jedhy--fontify-eldoc))

(defun hy-jedhy--candidate-str->full-docs (candidate-str)
  "Get full, multi-line docs for a CANDIDATE-STR."
  (-some->>
   candidate-str
   (format "(--JEDHY.full-docs \"%s\")")
   hy-shell--redirect-send-internal
   hy-jedhy--format-output-str
   s-chomp
   hy-jedhy--fontify-first-docs-line
   hy-jedhy--format-describe-output))

;;; Describe thing at point

(defun hy-describe-thing-at-point ()
  "Describe symbol at point with help popup buffer.

Retrieves full documentation, with firstline formatted same as eldoc, in a
popup buffer.

Does not (yet) complete the dot-dsl like Eldoc does currently.

Spacemacs users maybe be familiar with this functionality via
shift-K keybinding that executes `spacemacs/evil-smart-doc-lookup'."
  (interactive)
  (-when-let (text (hy-jedhy--candidate-str->full-docs (thing-at-point 'symbol)))
    (unless (s-blank-str? text)
      (with-current-buffer (get-buffer-create hy-jedhy--doc-lookup-buffer)
        (erase-buffer)
        (switch-to-buffer-other-window hy-jedhy--doc-lookup-buffer)

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

;;; Eldoc

(defun hy-eldoc-documentation-function ()
  "Drives `eldoc-mode', retrieves eldoc msg string for inner-most symbol."
  (hy-jedhy--candidate-str->eldoc (hy-jedhy--get-inner-symbol)))

;;; Company

(defun company-hy (command &optional prefix-or-candidate-str &rest ignored)
  "Implements autocompletion for `hy-mode'."
  (interactive (list 'interactive))

  (when (and (memq major-mode '(hy-mode inferior-hy-mode))
             (hy-shell--live-internal?))
    (cl-case command
      (prefix (unless (company-in-string-or-comment)
                (company-grab-symbol)))
      (candidates (hy-jedhy--prefix-str->candidates prefix-or-candidate-str))
      (annotation (hy-jedhy--candidate-str->annotation prefix-or-candidate-str))
      (meta (hy-jedhy--candidate-str->eldoc prefix-or-candidate-str)))))

;;; Run Jedhy

(defun run-jedhy--pyvenv-post-deactive-hook ()
  "Kill jedhy without notifying and possibly rerun for global context."
  (let ((hy-shell--notify?))
    (hy-shell--kill-internal)

    ;; The activation hook handles switching the environment rerunning jedhy
    (unless pyvenv-virtual-env-name
      (run-jedhy))))

;;;###autoload
(defun run-jedhy ()
  "Startup internal Hy interpreter process, enabling jedhy for `company-mode'."
  (interactive)

  (hy-shell--with-internal
    (hy-jedhy--startup)))

;;; Provide:

(provide 'hy-jedhy)

;;; hy-jedhy.el ends here
