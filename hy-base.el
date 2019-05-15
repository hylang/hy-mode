;;; hy-base.el --- Common Utils -*- lexical-binding: t -*-

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

;; Common requires and utilities for `hy-mode'.

;;; Code:

(require 'cl)
(require 'dash)
(require 'dash-functional)
(require 's)

;;; Syntax State Aliases

;; Alias `parse-partial-sexp' and `syntax-ppss' components

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

(defun hy--prior-sexp? (state)
  "Is there a prior sexp from syntax STATE?"
  (number-or-marker-p (hy--start-of-last-sexp state)))

;;; General Purpose

(defun hy--str-or-nil (text)
  "If TEXT is non-blank, return TEXT else nil."
  (and (not (s-blank? text)) text))

(defun hy--str-or-empty (text)
  "Return TEXT or the empty string it TEXT is nil."
  (if text text ""))

;;; Provide:

(provide 'hy-base)
