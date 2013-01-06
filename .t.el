;;; save-sexp/.t.el --- tests for library `save-sexp'

;;; Commentary:

;; Some early (and fairly incomplete) tests for save-sexp.el.

;; What is being tested:
;; - basic use of save-sexp-save-setq
;; - modification of current buffer
;; - basic adding and updating of sexps
;; - whitespace around inserted and removed sexps
;; - inserting before final comment and provide sexp

;; What isn't tested yet:
;; - save-sexp-save
;; - save-sexp-save-NOTSETQ
;; - inserting doc-strings
;; - optional arguments to control details of insertion
;; - interactive use of save-sexp-save-ANY
;; - modification of file or explicit buffer
;; - ...

;; Ert does not really make this as easy as it should,
;; or maybe I just haven't done this often enough yet.

;;; Code:

(require 'ert)
(require 'save-sexp)

(defun save-sexp--bs ()
  (buffer-substring-no-properties (point-min) (point-max)))

(ert-deftest save-sexp--insert--plain ()
  "Test inserting new sexps into empty buffer."
  (let ((a 'a) (b 'b))
    (with-temp-buffer
      (save-sexp-save-setq nil 'a)
      (should (equal (save-sexp--bs)
                     "(setq a 'a)\n"))
      (save-sexp-save-setq nil 'b)
      (should (equal (save-sexp--bs)
                     "(setq a 'a)\n\n(setq b 'b)\n"))
      )))

(ert-deftest save-sexp--insert--before-comment ()
  "Test inserting new sexps into buffer with final comment."
  (let ((a 'a) (b 'b))
    (with-temp-buffer
      (insert ";;;eof\n")
      (save-sexp-save-setq nil 'a)
      (should (equal (save-sexp--bs)
                     "(setq a 'a)\n\n;;;eof\n"))
      (save-sexp-save-setq nil 'b)
      (should (equal (save-sexp--bs)
                     "(setq a 'a)\n\n(setq b 'b)\n\n;;;eof\n"))
      (erase-buffer)
      (insert ";;;eof")
      (save-sexp-save-setq nil 'a)
      (should (equal (save-sexp--bs)
                     "(setq a 'a)\n\n;;;eof\n"))
      (save-sexp-save-setq nil 'b)
      (should (equal (save-sexp--bs)
                     "(setq a 'a)\n\n(setq b 'b)\n\n;;;eof\n"))
      )))

(ert-deftest save-sexp--insert--before-require ()
  "Test inserting new sexps into buffer providing feature."
  (let ((a 'a) (b 'b))
    (with-temp-buffer
      (insert "(provide 'f)\n")
      (save-sexp-save-setq nil 'a)
      (should (equal (save-sexp--bs)
                     "(setq a 'a)\n\n(provide 'f)\n"))
      (save-sexp-save-setq nil 'b)
      (should (equal (save-sexp--bs)
                     "(setq a 'a)\n\n(setq b 'b)\n\n(provide 'f)\n"))
      (erase-buffer)
      (insert "(provide 'f)")
      (save-sexp-save-setq nil 'a)
      (should (equal (save-sexp--bs)
                     "(setq a 'a)\n\n(provide 'f)\n"))
      (save-sexp-save-setq nil 'b)
      (should (equal (save-sexp--bs)
                     "(setq a 'a)\n\n(setq b 'b)\n\n(provide 'f)\n"))
      )))

(ert-deftest save-sexp--update--plain ()
  "Test updating sexps."
  (let ((a 'a))
    (with-temp-buffer
      (save-sexp-save-setq nil 'a)
      (should (equal (save-sexp--bs)
                     "(setq a 'a)\n"))
      (save-sexp-save-setq nil 'a)
      (should (equal (save-sexp--bs)
                     "(setq a 'a)\n"))
      (setq a 'A)
      (save-sexp-save-setq nil 'a)
      (should (equal (save-sexp--bs)
                     "(setq a 'A)\n"))
      )))

(ert-deftest save-sexp--replace--multi ()
  "Test updating sexps that are defined multiple times in buffer.
Also test if [lack of] whitespace around removed/updated sexps is
preserved."
  (let ((a 'a))
    (with-temp-buffer
      (insert "(setq a 'a)\n\n(setq a 'a)\n")
      (save-sexp-save-setq nil 'a)
      (should (equal (save-sexp--bs)
                     "(setq a 'a)\n"))
      (erase-buffer)
      (insert "(setq a 'a)\n\n(setq b 'b)\n(setq a 'a)\n\n(setq a 'a)\n(setq b 'b)\n\n(setq a 'a)\n\n(setq b 'b)\n")
      (save-sexp-save-setq nil 'a)
      (should (equal (save-sexp--bs)
                     "(setq a 'a)\n\n(setq b 'b)\n\n(setq b 'b)\n\n(setq b 'b)\n"))
      )))

;;; intentionally no feature provided
;; Local Variables:
;; no-byte-compile: t
;; indent-tabs-mode: nil
;; End:
;;; save-sexp/.t.el ends here
