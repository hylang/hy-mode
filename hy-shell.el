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

(defvar hy-shell--output-filter-in-progress nil
  "Whether we are waiting for output in `hy-shell-send-string-no-output'.")

;;; Provide:

(provide 'hy-shell)
