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

;;; Fundamentals
;;;; Accessing

(defun hy-shell--installed? ()
  "Is the command `hy-shell--interpreter' available?"
  (executable-find hy-shell--interpreter))

(defun hy-shell--get-buffer-create ()
  "Get or create `hy-shell--buffer'."
  (setq hy-shell--buffer
        (get-buffer-create hy-shell--buffer-name)))

(defun hy-shell--get-buffer-create-internal ()
  "Get or create `hy-shell--buffer-internal'."
  (setq hy-shell--buffer-internal
        (get-buffer-create hy-shell--buffer-name-internal)))

(defun hy-shell--format-proc-name (proc-name)
  "Enclose PROC-NAME with astericks to follow process naming conventions."
  (s-concat "*" proc-name "*"))

(defun hy-shell--process ()
  "Process corr. to `hy-shell--buffer-name'."
  (get-process hy-shell--buffer-name))

(defun hy-shell--process-internal ()
  "Process corr. to `hy-shell--buffer-name-internal'."
  (get-process hy-shell--buffer-name-internal))

;;;; Status

(defun hy-shell--live? ()
  "Is Hy's shell alive?"
  (process-live-p (hy-shell--process)))

(defun hy-shell--buffer-live? ()
  "Is Hy's shell buffer alive?"
  (when hy-shell--buffer
    (buffer-live-p hy-shell--buffer)))

(defun hy-shell--live-internal? ()
  "Is Hy's internal process alive?"
  (process-live-p (hy-shell--process-internal)))

(defun hy-shell--buffer-live-internal? ()
  "Is Hy's shell buffer alive?"
  (when hy-shell--buffer-internal
    (buffer-live-p hy-shell--buffer-internal)))

;;;; Commands

(defun hy-shell--kill ()
  "Kill `hy-shell--buffer'."
  (when (hy-shell--buffer-live?)
    (kill-buffer hy-shell--buffer)
    (when (derived-mode-p 'inferior-hy-mode)
      (setq hy-shell--buffer nil))))

(defun hy-shell--kill-internal ()
  "Kill `hy-shell--buffer-internal'."
  (when (hy-shell--buffer-live-internal?)
    (kill-buffer hy-shell--buffer-internal)
    (when (derived-mode-p 'inferior-hy-mode)
      (setq hy-shell--buffer-internal nil))))

;;; Provide:

(provide 'hy-shell)
