;;; hy-base.el --- Common Utils -*- lexical-binding: t -*-

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

;; Common requires and utilities for `hy-mode'.

;;; Code:

(require 'cl)
(require 'dash)
(require 'dash-functional)
(require 's)

;;; Syntax State Methods
;;;; Alias `syntax-ppss' and `parse-partial-sexp'

(defun hy--syntax->inner-char (syntax)
  "Get innermost char of SYNTAX."
  (nth 1 syntax))

(defun hy--syntax->last-sexp-start (state)
  "Return start of last sexp of syntax STATE."
  (nth 2 state))

(defun hy--syntax->string-start (syntax)
  "Return start of STATE that is in a string."
  (nth 8 syntax))

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

;;;; Utilities

(defun hy--syntax->inner-symbol (syntax)
  "Get innermost sexp of SYNTAX."
  (save-excursion
    (when (hy--goto-inner-sexp syntax)
      (thing-at-point 'symbol))))

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

;;; Legacy

(defun hy--sexp-inermost-char (state)
  "Return innermost char of syntax STATE."
  (nth 1 state))

(defun hy--start-of-last-sexp (state)
  "Return start of last sexp of syntax STATE."
  (nth 2 state))

(defun hy--in-string? (state)
  "Is syntax STATE in a string?"
  (nth 3 state))

(defun hy--in-string-or-comment? (state)
  "Is syntax STATE in a string or comment?"
  (or (nth 3 state) (nth 4 state)))

(defun hy--start-of-string (state)
  "Return start of syntax STATE that is in a string."
  (nth 8 state))

;;;; Methods

(defun hy--prior-sexp? (state)
  "Is there a prior sexp from syntax STATE?"
  (number-or-marker-p (hy--start-of-last-sexp state)))

;;;; General Purpose

(defun hy--str-or-nil (text)
  "If TEXT is non-blank, return TEXT else nil."
  (and (not (s-blank? text)) text))

(defun hy--str-or-empty (text)
  "Return TEXT or the empty string it TEXT is nil."
  (if text text ""))

;;; Provide:

(provide 'hy-base)
