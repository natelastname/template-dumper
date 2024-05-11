;;; -*- mode: emacs-lisp; lexical-binding: t; -*-
;; Created on 2024-05-08T16:38:32-04:00
;; @author: nate

(require 'yasnippet)
(require 'tpl-dumper)
(require 'f)


(defun tpl-dumper-dump-simple (file-path tpl-name)
  "Create file FILE-PATH from yas template TPL-NAME."
  (let ((file-contents
         (with-temp-buffer
           (yas-minor-mode 1)
           (tpl-dumper-insert-yas-by-name tpl-name)
           (buffer-substring-no-properties (point-min) (point-max)))))
    (f-write-text file-contents 'utf-8 file-path)))


(defun mk-file-tpl (file-path tpl-name)
  "Create file FILE-PATH from Yasnippet template TPL-NAME.
Does not overwrite existing files and creates missing parent
directories as neccessary."
  (if (f-exists-p file-path)
      (message
       (format "[tpl-dumper] File '%s' already exists, skipping..."
               file-path))
    (progn
      (message (format
                "[tpl-dumper] Making file '%s' from template '%s'"
                file-path
                tpl-name))
      (tpl-dumper-mkdir-p (f-dirname file-path))
      (tpl-dumper-dump-simple file-path tpl-name)
      )))

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
    (mk-file-tpl abs-path tpl-name)))

(defun tpl-dumper--mk-rel-file (basepath file-spec)
  "Make file-spec FILE-SPEC relative to BASEDIR."
  (let* ((path-orig (nth 1 file-spec))
        (new-path (f-join basepath path-orig)))
    (cons 'file (cons new-path (cdr (cdr file-spec))))))

;; ###################################################################
;; These two functions are not used internally, the struture of a
;; path-spec is determined by `tpl-dumper-mk-tree'.
;; ###################################################################

(defun tpl-dumpler-is-path-spec (tree)
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
    (mk-file-tpl abs-path tpl-name)))


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
        ((tpl-dumpler-is-path-spec spec)
         (tpl-dumper--mk-rel-path basepath spec))
        (t
         (error "[tpl-dumper] Expected a path-spec or a file-spec"))))

(defun tpl-dumper-mk-tree (tree)
  "Create a project from a project template spec TREE."
  (interactive)
  (cond ((tpl-dumper-is-file-spec tree)
         (tpl-dumper-dump-file-spec tree))
        ((tpl-dumpler-is-path-spec tree)
         (let* ((basepath (car (cdr tree)))
                (tree0 (car (cdr (cdr tree)))))
           (dolist (item tree0)
               ;; Update any paths to be relative to `basepath' then
               ;; recurse on each directory item
               (tpl-dumper-mk-tree (tpl-dumper-mk-rel basepath item))
               )))
        (t
         (error "[tpl-dumper] Expected a path-spec or a file-spec"))))


(let ((fspec '(file "README.org" "ord-default-readme")))
  (cl-assert (tpl-dumper-is-file-spec fspec)))

(let ((tree-py-proj '(dir "/home/nate/spyder_projects/pyapropos3"
                          ((file "main.py" "python-default-file")
                           (file "README.org" "org-default-readme")
                           (file "LICENSE" "license-expat")
                           (file ".gitignore" "gitignore-default-file")))))
  (cl-assert (tpl-dumpler-is-path-spec tree-py-proj)))


;;(tpl-dumper-dump-simple "/home/nate/tmp.txt" "org-default-file")
;;(tpl-dumper-dump-simple "/home/nate/tmp.txt" "python-default-file")
