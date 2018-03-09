(require 'comint)
(require 'faceup)
(require 'ert)
(require 'dash)
(require 's)

(add-to-list 'load-path
             (-> (f-this-file) (f-parent) (f-parent)))
(require 'hy-mode)


;;; Macros

(defmacro hy-with-hy-mode (&rest forms)
  "Execute FORMS in a temporary `hy-mode' buffer."
  `(with-temp-buffer
     (hy-mode)
     ,@forms))


(defmacro hy-with-hy-shell (&rest forms)
  "Execute FORMS with an active hy process."
  `(-let [hy-shell-interpreter-args ""]
     (hy-shell-kill)
     (save-window-excursion (run-hy))
     (set-process-query-on-exit-flag (hy-shell-get-process) nil)
     ,@forms
     (hy-shell-kill)))


(defmacro hy-with-hy-shell-internal (&rest forms)
  "Execute FORMS with an active hy internal process."
  `(progn
     (hy-shell-kill)
     (run-hy-internal)
     ,@forms
     (hy-shell-kill)))


;;; Assertions

(defun s-assert (string1 string2)
  "Combine `should' and `s-equals?'."
  (should (s-equals? string1 string2)))


(defun hy--assert-indented (text)
  "Assert indenting the left-trimmed version of TEXT matches TEXT."
  (hy-with-hy-mode
   (-let [no-indents
          (->> text s-lines (-map 's-trim-left) (s-join "\n"))]
     (insert no-indents)
     (indent-region (point-min) (point-max))

     (s-assert (buffer-substring-no-properties (point-min) (point-max))
               text))))


(defun hy--font-lock-test (text)
  "Entry to test faces of TEXT markedup as faceup, disabling minor modes faces.

See `faceup-face-short-alist' for faceup's face aliases."
  (when (fboundp 'rainbow-delimiters-mode-disable)
    (advice-add 'hy-mode :after 'rainbow-delimiters-mode-disable))
  (prog1
      (faceup-test-font-lock-string 'hy-mode text)
    (advice-remove 'hy-mode 'rainbow-delimiters-mode-disable)))
(faceup-defexplainer hy--font-lock-test)
(defun hy--assert-faces (text)
  "Assert text props of TEXT according to `faceup' markup."
  (should (hy--font-lock-test text)))


(defun hy--assert-current-form-string (form-string)
  "Assert FORM-STRING is extracted by `hy--current-form-string'."
  (hy-with-hy-mode
   (insert form-string)
   (forward-char -1)
   (s-assert form-string
             (hy--current-form-string))))
