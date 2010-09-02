;;; save-sexp.el --- replace S-expressions in files to save variables

;; Copyright (C) 2010  Jonas Bernoulli
;; Copyright (C) 1996, 1997, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
;;   2006, 2007, 2008, 2009, 2010  Free Software Foundation, Inc.

;; Author: Jonas Bernoulli <jonas@bernoul.li>
;; Created: 20100902
;; Updated: 20100902
;; Version: 0.1
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
;;   (save-sexp-save 'some-variable "/some/file.el"
;;                    'save-sexp-save-setq-1 'pp-to-string)

;; But since this is the usecase for which this library was created a
;; shortcut exists (which is also an interactive command):
;;
;;   (save-sexp-save-setq 'some-variable "/some/file.el")

;; If on the other hand you want to use another form than `setq' this
;; gets you started:
;;
;;   (save-sexp-save 'some-variable "/some/file.el"
;;                    (lambda (var pp)
;;                      (save-sexp-default-save 'defvar var pp 2))
;;                    'pp-to-string)

;;; Code:

(defun save-sexp-save-setq (variable file)
  "Save the current value of VARIABLE in FILE using a `setq' form.
The value of VARIABLE is pretty-printed using function `pp-to-string'."
  (interactive (list (read-variable "Save variable: ")
		     (read-file-name "in file: ")))
  (save-sexp-save variable file 'save-sexp-save-setq-1 'pp-to-string))

(defun save-sexp-save (variable file save &optional pp)
  "Save the value of VARIABLE in FILE using SAVE.

This function prepares a buffer to edit FILE and then calls function SAVE
with VARIABLE and PP as arguments.  SAVE should remove existing forms
which set the value or VARIABLE (using `save-sexp-delete') and then insert
a new form that (re)sets VARIABLE to it's current value when it is later
evaluated.

Argument PP is optional here but the SAVE function has to accept it as
the second argument, though it may ignore it.

See function `save-sexp-save-setq' for a function that can be used as
SAVE.  Also note that the the string returned by function PP should be
additionally indended by SAVE before being inserted into the buffer;
function `save-sexp-indent' can be used for this purpose."
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
	(funcall save variable pp))
      (let ((file-precious-flag t))
	(save-buffer))
      (if old-buffer
	  (progn
	    (set-visited-file-name old-buffer-name)
	    (set-buffer-modified-p nil))
	(kill-buffer (current-buffer))))))

(defun save-sexp-save-setq-1 (variable &optional pp)
  "Insert into the current buffer a `setq' form which sets VARIABLE.
The value of VARIABLE is pretty-printed using function PP or if is is
non-nil."
  (save-sexp-save variable file
		   (lambda (var pp)
		     (save-sexp-default-save 'setq var pp 5))
		   pp))

(defun save-sexp-default-save (setter variable &optional pp indent)
  "Insert into the current buffer a form which sets VARIABLE.
The inserted S-expression begins with SETTER, followed by unquoted
VARIABLE, and the value of VARIABLE which is pretty-printed using
function PP if it is non-nil `pp-to-string'."
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
      (princ ")")
      (unless (looking-at "\n")
	(princ "\n")))))

(defun save-sexp-delete (predicate)
  "Remove all S-expressions matching PREDICATE from the current buffer."
  (goto-char (point-min))
  ;; Skip all whitespace and comments.
  (while (forward-comment 1))
  (or (eobp)
      (save-excursion (forward-sexp (buffer-size)))) ; Test for scan errors.
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
	  (when (search-forward "Local Variables:" nil t)
	    (setq pos (line-beginning-position))))
	(goto-char pos)))))

(defun save-sexp-pp-indent (string indent)
  (replace-regexp-in-string
   "^\\([\s\t]+\\|\\)[^\n]"
   (lambda (str)
     (save-match-data
       (string-match "\\(\t+\\)?\\(\s+\\)?" str)
       (let ((len (+ (* 8 (length (match-string 1 str)))
		     (length (match-string 2 str))
		     indent)))
	 (concat (make-string (/ len 8) ?\t)
		 (make-string (% len 8) ?\s)))))
   string nil nil 1))

(provide 'save-sexp)
;;; save-sexp.el ends here
