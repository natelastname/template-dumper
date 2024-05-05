;;; tpl-dumper.el --- Create files from yas-snippet templates -*- lexical-binding: t -*-

;; Author: Nathan Nichols
;; Maintainer: Nathan Nichols
;; Version: 1.0
;; Package-Requires: ((emacs "28.1") (yasnippet))
;; Package
;; Homepage: https://resultsmotivated.com/
;; Keywords: yasnippet templating


;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.


;;; Commentary:

;; Create new files with yas-snippet templates.  Also has some
;; utilities for creating files with dynamically generated file names.

;;; Code:

;;; -*- mode: emacs-lisp; -*-
;; Created on 2023-12-23T10:33:01-05:00
;; @author: nate

(defun tpl-dumper-insert-yas-by-name (name)
  "Insert Yas-snippet NAME at point."
  (flet ((dummy-prompt
          (prompt choices &optional display-fn)
          (declare (ignore prompt))
          (or (find name choices :key display-fn :test #'string=)
              (throw 'notfound nil))))
    (let ((yas-prompt-functions '(dummy-prompt)))
      (catch 'notfound
        (yas-insert-snippet t)))))

(defun tpl-dumper-mkdir-p (dirname)
  "Create a directory DIRNAME if one does not exist."
  (if (not (file-exists-p dirname))
      (make-directory dirname t)))

(defun tpl-dumper-str-or-fn-callback (input)
  "Evaluate INPUT as a function, or convert to a string."
  (cond ((functionp input) (format "%s" (funcall input)))
        ((stringp input) input)
        (t (format "%s" input))))

(cl-assert (string= (tpl-dumper-str-or-fn-callback (lambda () "abcdefg")) "abcdefg"))
(cl-assert (string= (tpl-dumper-str-or-fn-callback "abcdefg") "abcdefg"))
(cl-assert (string= (tpl-dumper-str-or-fn-callback 123) "123"))

(defun tpl-dumper-new-named-file (outdir callback &optional ext)
  "Create a file `[OUTDIR]/[CALLBACK].[EXT]`.
CALLBACK is a string or a function that returns a string."
  (tpl-dumper-mkdir-p outdir)
  (let* ((ts (tpl-dumper-str-or-fn-callback callback))
         (fpath (file-name-concat outdir (concat ts ext))))
    (find-file fpath)
    (message fpath)))

(defun tpl-dumper-yas-new-file (basedir yas-tpl name-callback &optional ext)
  "Create a file with extension EXT from yas-snippet YAS-TPL in BASEDIR.
Return the path of the newly-created file."
  (let* ((with-dot (cond
                    ((not ext) "")
                    ((equal (length ext) 0) "")
                    ((equal (substring ext 0 1) ".") ext)
                         (t (concat "." ext))))
         (new-file (tpl-dumper-new-named-file basedir name-callback with-dot)))
    (tpl-dumper-insert-yas-by-name yas-tpl)
    new-file))


(provide 'tpl-dumper)
;;; tpl-dumper.el ends here
