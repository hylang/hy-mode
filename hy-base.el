;;; hy-base.el --- Common Utils -*- lexical-binding: t -*-

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

;; Common requires and utilities for `hy-mode'.

;; Mostly just methods for syntax states and sexp traversals.

;;; Code:

(require 'cl-lib)
(require 'dash)
(require 'dash-functional)
(require 's)

;;; Syntax Methods
;;;; `syntax-ppss' and `parse-partial-sexp' aliases
;;;;; Positions

(defun hy--syntax->inner-char (syntax)
  "Get innermost char of SYNTAX."
  (nth 1 syntax))

(defun hy--syntax->last-sexp-start (state)
  "Return start of last sexp of syntax STATE."
  (nth 2 state))

(defun hy--syntax->string-start (syntax)
  "Return start of STATE that is in a string."
  (nth 8 syntax))

(defun hy--syntax->inner-symbol (syntax)
  "Get innermost sexp of SYNTAX."
  (save-excursion
    (when (hy--goto-inner-sexp syntax)
      (thing-at-point 'symbol))))

;;;;; Predicates

(defun hy--in-string? (state)
  "Is syntax STATE in a string?"
  (nth 3 state))

(defun hy--in-string-or-comment? (state)
  "Is syntax STATE in a string or comment?"
  (or (nth 3 state) (nth 4 state)))

(defun hy--prior-sexp? (state)
  "Is there a prior sexp from syntax STATE?"
  (number-or-marker-p (hy--syntax->last-sexp-start state)))

;;;; Gotos

(defun hy--goto-inner-char (syntax)
  "Goto innermost char of SYNTAX."
  (-some-> syntax hy--syntax->inner-char goto-char))

(defun hy--goto-inner-sexp (syntax)
  "Goto innermost sexp of SYNTAX."
  (-some-> syntax hy--syntax->inner-char 1+ goto-char))

(defun hy--goto-last-sexp-start (syntax)
  "Goto start of last sexp of SYNTAX."
  (-some-> syntax hy--syntax->last-sexp-start goto-char))

;;; Form Captures

(defun hy--current-form-string ()
  "Get form containing current point as string plus a trailing newline."
  (save-excursion
    (-when-let (start (hy--goto-inner-char (syntax-ppss)))
      (while (ignore-errors (forward-sexp)))

      (s-concat (buffer-substring-no-properties start (point))
                "\n"))))

(defun hy--last-sexp-string ()
  "Get form containing last s-exp point as string plus a trailing newline."
  (save-excursion
    (-when-let (start (hy--goto-last-sexp-start (syntax-ppss)))
      (while (ignore-errors (forward-sexp)))

      (s-concat (buffer-substring-no-properties start (point))
                "\n"))))

;;; Provide:

(provide 'hy-base)

;;; hy-base.el ends here
