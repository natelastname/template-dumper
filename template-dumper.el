;;; tpl-dumper.el --- Create files from yasnippet templates -*- lexical-binding: t -*-

;; Author: Nathan Nichols
;; Maintainer: Nathan Nichols
;; Version: 1.0
;; Package-Requires: ((emacs "28.1") (yasnippet "0.14.0") (f "0.20.0"))
;; Package
;; Homepage: https://resultsmotivated.com/
;; Keywords: yasnippet templating convenience tools


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

(require 'emacs)
(require 'cl-lib)
(require 'yasnippet)
(require 'f)

(defun tpl-dumper-insert-yas-by-name (name)
  "Insert Yas-snippet NAME at point."
  (catch 'notfound
    (let ((yas-prompt-functions
           (list (lambda (_prompt choices &optional display-fn)
                   (or (cl-find name choices :key display-fn :test #'string=)
                       (throw 'notfound nil))))))
      (yas-insert-snippet t))))

(defun tpl-dumper-mkdir-p (dirname)
  "Create a directory DIRNAME if one does not exist."
  (if (not (file-exists-p dirname))
      (make-directory dirname t)))

(defun tpl-dumper-str-or-fn-callback (input)
  "Evaluate INPUT as a function, or convert to a string."
  (cond ((functionp input) (format "%s" (funcall input)))
        ((stringp input) input)
        (t (format "%s" input))))

(defun tpl-dumper-new-named-file (outdir callback &optional ext)
  "Create a file `[OUTDIR]/[CALLBACK].[EXT]`.
CALLBACK is a string or a function that returns a string."
  (tpl-dumper-mkdir-p outdir)
  (let* ((ts (tpl-dumper-str-or-fn-callback callback))
         (fpath (f-join outdir (concat ts ext))))
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


;; ###################################################################
;; Functionality for creating projects from templates
;; ###################################################################

(defun tpl-dumper-dump-simple (file-path tpl-name)
  "Create file FILE-PATH from yas template TPL-NAME."
  (let ((file-contents
         (with-temp-buffer
           (yas-minor-mode 1)
           (tpl-dumper-insert-yas-by-name tpl-name)
           (buffer-substring-no-properties (point-min) (point-max)))))
    (f-write-text file-contents 'utf-8 file-path)))


(defun tpl-dumper-mk-file-tpl (file-path tpl-name)
  "Create file FILE-PATH from Yasnippet template TPL-NAME.
Does not overwrite existing files and creates missing parent
directories as neccessary."
  (if (f-exists-p file-path)
      (message "[tpl-dumper] File '%s' already exists, skipping..."
               file-path)
    (progn
      (message "[tpl-dumper] Making file '%s' from template '%s'"
                file-path
                tpl-name)
      (tpl-dumper-mkdir-p (f-dirname file-path))
      (tpl-dumper-dump-simple file-path tpl-name))))

;; ###################################################################
;; These functions determine the metadata/structure of a file-spec.
;; ###################################################################

(defun tpl-dumper-is-file-spec (tree)
  "Whether TREE is a file-spec."
  (and (listp tree)
       (eql 'file (car tree))
       (cl-every #'stringp (cdr tree))))

(defun tpl-dumper-dump-file-spec (spec)
  "Create a file from a file-spec SPEC."
  (let* ((abs-path (nth 1 spec))
         (tpl-name (nth 2 spec)))
    (tpl-dumper-mk-file-tpl abs-path tpl-name)))

(defun tpl-dumper--mk-rel-file (basepath file-spec)
  "Make file-spec FILE-SPEC relative to BASEPATH."
  (let* ((path-orig (nth 1 file-spec))
        (new-path (f-join basepath path-orig)))
    (cons 'file (cons new-path (cdr (cdr file-spec))))))

;; ###################################################################
;; cmd-spec
;; ###################################################################

(defun tpl-dumper-is-cmd-spec (tree)
  "Whether TREE is a file-spec."
  (and (listp tree)
       (eql 'cmd (car tree))
       (cl-every #'stringp (cdr tree))))


(defun tpl-dumper--run-cmd (abs-path cmd)
  "Run CMD in a subprocess shell from working directory ABS-PATH."
  (message "[tpl-dumper] %s: %s" abs-path cmd)
  (tpl-dumper-mkdir-p abs-path)
  (let* ((default-directory abs-path)
         (output (with-temp-buffer
           (with-environment-variables (("DIR" abs-path))
             (shell-command cmd (current-buffer))
             (buffer-substring (point-min) (point-max))))))
    (message output)
    output))

(defun tpl-dumper-exec-cmd-spec (spec)
  "Execute cmd-spec SPEC."
  (let* ((abs-path (nth 1 spec))
         (cmd (nth 2 spec)))
    (tpl-dumper--run-cmd abs-path cmd)))

(defun tpl-dumper--mk-rel-cmd (basepath cmd-spec)
  "Make cmd-spec CMD-SPEC relative to BASEPATH."
  (let ((with-path (if (eql (length cmd-spec) 2)
                       (cons 'cmd (cons "./" (cdr cmd-spec)))
                     cmd-spec)))
    (let* ((path-orig (nth 1 with-path))
           (new-path (f-join basepath path-orig)))
      (cons 'cmd (cons new-path (cdr (cdr with-path)))))))

;; ###################################################################
;; These two functions are not used internally, the struture of a
;; path-spec is determined by `tpl-dumper-mk-tree'.
;; ###################################################################

(defun tpl-dumper-is-path-spec (tree)
  "Whether TREE is a path-spec.
A directory spec is a list starting with the symbol `dir'
followed by a string (the path) and then 0 or more lists."
  (and (listp tree)
       (eql 'dir (car tree))
       (stringp (car (cdr tree)))
       (cl-every #'listp (cdr (cdr tree)))))

(defun tpl-dumper-dump-path-spec (spec)
  "Create a directory from a path-spec SPEC."
  (let* ((abs-path (nth 1 spec))
         (tpl-name (nth 2 spec)))
    (tpl-dumper-mk-file-tpl abs-path tpl-name)))

(defun tpl-dumper--mk-rel-path (basepath path-spec)
  "Make path-spec PATH-SPEC relative to BASEPATH."
  (let* ((path-orig (car (cdr path-spec)))
        (new-path (f-join basepath path-orig)))
    (cons 'dir (cons new-path (cdr (cdr path-spec))))))

;; ###################################################################

(defun tpl-dumper-mk-rel (basepath spec)
  "Make SPEC relative to BASEPATH.
SPEC is either a file-spec or a path-spec."
  (cond ((tpl-dumper-is-file-spec spec)
         (tpl-dumper--mk-rel-file basepath spec))
        ((tpl-dumper-is-cmd-spec spec)
         (tpl-dumper--mk-rel-cmd basepath spec))
        ((tpl-dumper-is-path-spec spec)
         (tpl-dumper--mk-rel-path basepath spec))
        (t
         (error "[tpl-dumper] Expected a path-spec, cmd-spec or file-spec"))))

(defun tpl-dumper-mk-tree (tree)
  "Create a project from a project template spec TREE."
  (interactive)
  (cond ((tpl-dumper-is-file-spec tree)
         (tpl-dumper-dump-file-spec tree))
        ((tpl-dumper-is-cmd-spec tree)
         (tpl-dumper-exec-cmd-spec tree))
        ((tpl-dumper-is-path-spec tree)
         (let* ((basepath (car (cdr tree)))
                (tree0 (car (cdr (cdr tree)))))
           (dolist (item tree0)
             ;; Update any paths to be relative to `basepath' then
             ;; recurse on each directory item
             (tpl-dumper-mk-tree
              (tpl-dumper-mk-rel basepath item)))))
        (t
         (error "[tpl-dumper] Expected a path-spec, cmd-spec or file-spec"))))

(defun tpl-dumper-mk-proj-tree-rel (tree)
  "Create project from template tree TREE.
User is prompted for the directory under which the template tree is created."
  (interactive)
  (let* ((path (read-directory-name "Enter directory:"))
         (tree-rel (tpl-dumper-mk-rel path tree)))
    (tpl-dumper-mk-tree tree-rel)))

;; ###################################################################
;; Extremely basic self-tests
;; ###################################################################
(cl-assert (string= (tpl-dumper-str-or-fn-callback (lambda () "abcdefg")) "abcdefg"))
(cl-assert (string= (tpl-dumper-str-or-fn-callback "abcdefg") "abcdefg"))
(cl-assert (string= (tpl-dumper-str-or-fn-callback 123) "123"))

(provide 'tpl-dumper)
;;; tpl-dumper.el ends here
