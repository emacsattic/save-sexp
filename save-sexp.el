;;; save-sexp.el --- replace S-expressions in files to save variables

;; Copyright (C) 2010-2012  Jonas Bernoulli
;; Copyright (C) 1996-1997, 1999-2010  Free Software Foundation, Inc.

;; Author: Jonas Bernoulli <jonas@bernoul.li>
;; Created: 20100902
;; Version: 0.1.1
;; Homepage: https://github.com/tarsius/save-sexp
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Support for replacing S-expressions in files to save variables similar
;; to how easy Customize does it.  In fact most of the code is taken from
;; `cus-edit.el' and is just generalized a bit for use outside Customize.

;; A variable's value can be saved like this:
;;
;;   (save-sexp-save "/some/file.el" 'save-sexp-save-setq-1
;;                   'some-variable 'pp-to-string)

;; But since this is the usecase for which this library was created a
;; shortcut exists (which is also an interactive command):
;;
;;   (save-sexp-save-setq 'some-variable "/some/file.el")

;; If on the other hand you want to use another form than `setq' this
;; gets you started:
;;
;;   (save-sexp-save "/some/file.el"
;;                   (lambda (var pp)
;;                     (save-sexp-default-save 'defvar var pp 2))
;;                   'some-variable 'pp-to-string)

;;; Code:

(defun save-sexp-save-setq (variable file)
  "Save the current value of VARIABLE in FILE using a `setq' form.
The value of VARIABLE is pretty-printed using function `pp-to-string'."
  (interactive (list (read-variable "Save variable: ")
                     (read-file-name "in file: ")))
  (save-sexp-save file 'save-sexp-save-setq-1 variable 'pp-to-string))

(defun save-sexp-save (file save &rest args)
  "Save something specified by ARGS in FILE using SAVE.

This function only prepares a buffer to edit FILE and then calls
function SAVE with ARGS arguments.  That function is responsible
for actually saving the thing specified by ARGS after removing
existing forms which set the same thing.

Function `save-sexp-delete' can be used in SAVE to remove
existing forms.

Function `save-sexp-indent' can be used in SAVE to indent the
value part of the inserted value.

In many cases instead of using the above functions in SAVE
`save-sexp-default-save' can be used.

See function `save-sexp-save-setq-1' for a function that uses
`save-sexp-default-save' and can be used as SAVE."
  (let* ((recentf-exclude
          (if recentf-mode
              (cons (concat "\\`"
                            (regexp-quote
                             (recentf-expand-file-name custom-file))
                            "\\'")
                    recentf-exclude)))
         (old-buffer (find-buffer-visiting file))
         old-buffer-name)
    (with-current-buffer (let ((find-file-visit-truename t))
                           (or old-buffer (find-file-noselect file)))
      ;; We'll save using file-precious-flag, so avoid destroying
      ;; symlinks.  (If we're not already visiting the buffer, this is
      ;; handled by find-file-visit-truename, above.)
      (when old-buffer
        (setq old-buffer-name (buffer-file-name))
        (set-visited-file-name (file-chase-links file)))

      (unless (eq major-mode 'emacs-lisp-mode)
        (emacs-lisp-mode))
      (let ((inhibit-read-only t))
        (apply save args))
      (let ((file-precious-flag t))
        ;; TODO allow skipping this using global variable
        (save-buffer))
      (if old-buffer
          (progn
            (set-visited-file-name old-buffer-name)
            (set-buffer-modified-p nil))
        (kill-buffer (current-buffer))))))

(defun save-sexp-save-setq-1 (variable &optional pp)
  "Insert into the current buffer a `setq' form which sets VARIABLE.
If optional PP is non-nil it is used to pretty-printed VARIABLE's
value."
  (save-sexp-save file
                  (lambda (var pp)
                    (save-sexp-default-save 'setq var pp 5))
                  variable pp))

(defun save-sexp-default-save (setter variable &optional pp indent)
  "Insert into the current buffer a form which sets VARIABLE.
The inserted S-expression begins with SETTER, followed by unquoted
VARIABLE, and the value of VARIABLE which is pretty-printed using
function PP if it is non-nil."
  (save-excursion
    (save-sexp-delete
     (lambda (sexp)
       (and (eq (nth 0 sexp) setter)
            (eq (nth 1 sexp) variable))))
    (let ((standard-output (current-buffer))
          (value (symbol-value variable)))
      ;; Kludge.  Can this be done more gracefully?
      (when (memq (type-of value) '(symbol cons))
        (setq value (list 'quote value)))
      (unless (bolp)
        (princ "\n"))
      (princ (format "(%s %s" setter variable))
      (cond (pp (princ "\n")
                (princ (save-sexp-pp-indent (funcall pp value) 6)))
            (t  (princ " ")
                (prin1 value)))
      (when (looking-back "\n")
        (delete-char -1))
      (princ ")\n"))
    (unless (looking-at "\n")
      (princ "\n"))))

(defun save-sexp-delete (predicate)
  "Remove matching S-expressions from the current buffer.
Remove all top-level S-expressions from the current buffer for
which PREDICATE returns non-nil and move point to where the first
match was removed.  If nothing matches move point to the end of
the buffer or if there exist definitions of file local variables
just before those."
  (goto-char (point-min))
  ;; Skip all whitespace and comments.
  (while (forward-comment 1))
  (unless (eobp)
    ;; Test for scan errors.
    (save-excursion (forward-sexp (buffer-size))))
  (let (first)
    (catch 'found
      (while t ;; We exit this loop only via throw.
        ;; Skip all whitespace and comments.
        (while (forward-comment 1))
        (let ((start (point))
              (sexp (condition-case nil
                        (read (current-buffer))
                      (end-of-file (throw 'found nil)))))
          (when (and (listp sexp)
                     (funcall predicate sexp))
            (delete-region start (point))
            (unless first
              (setq first (point)))))))
    (if first
        (goto-char first)
      ;; Move in front of local variables, otherwise long
      ;; S-expressions would make them ineffective.
      (let ((pos (point-max))
            (case-fold-search t))
        (save-excursion
          (goto-char (point-max))
          (search-backward "\n\^L" (max (- (point-max) 3000) (point-min))
                           'move)
          ;; The `concat' works around a bug in Emacs.  Otherwise the
          ;; 'Local Variables' section would not be found by the code
          ;; responsible for setting the local variables.
          ;; FIXME that ^ seems unlikely
          (when (search-forward (concat "Local" " Variables:") nil t)
            (setq pos (line-beginning-position))))
        (goto-char pos)))))

(defun save-sexp-pp-indent (string indent)
  "Indent multi-line string STRING INDENT columns.
Each non-empty line is intended by indented by INDENT columns.
If `indent-tabs-mode' is nil use only spaces otherwise use spaces
and tab according to `tab-width'."
  (replace-regexp-in-string
   "^\\([\s\t]+\\|\\)[^\n]"
   (lambda (str)
     (save-match-data
       (string-match "\\(\t+\\)?\\(\s+\\)?" str)
       (let ((len (+ (* tab-width (length (match-string 1 str)))
                     (length (match-string 2 str))
                     indent)))
         (if indent-tabs-mode
             (concat (make-string (/ len tab-width) ?\t)
                     (make-string (% len tab-width) ?\s))
           (make-string len ?\s)))))
   string nil nil 1))

(provide 'save-sexp)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; save-sexp.el ends here
