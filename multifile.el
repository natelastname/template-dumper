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

(defun is-file-spec (tree)
  "TREE is a list containing two strings."
  (and (listp tree)
       (eql (length tree) 2)
       (cl-every #'stringp tree)))

(defun is-path-spec (tree)
  "TREE is a list containing one string followed by 0 or more lists."
  (and (listp tree)
       (stringp (car tree))
       (cl-every #'listp (cdr tree))))

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


(defun tpl-dumper-mk-tree (tree)
  (interactive)
  (if (is-file-spec tree)
      (mk-file-tpl (car tree) (car (cdr tree)))
    (let* ((basepath (car tree))
           (tree0 (car (cdr tree))))
      (dolist (tree1 tree0)
        (let ((newpath (f-join basepath (car tree1)))
              (newtail (cdr tree1)))
          (mk-tree (cons newpath newtail))
          )))))

;; ###################################################################
;; ###################################################################

(setq tree1 '("/home/nate/multifile-test/"
              (("file1" "tpl1")
               ("dir1" (("file2" "tpl2")
                        ("file3" "tpl3"))))))


(setq tree-test '("/home/nate/multifile-test/"
              (("file1" "tpl1")
               ("dir1" (("file2" "tpl2")
                        ("file3" "tpl3")
                        ("dir1" (("file2" "tpl2")
                                 ("file3" "tpl3"))))))))



(setq tree-py-proj '("/home/nate/spyder_projects/pyapropos3"
                     (("main.py" "python-default-file")
           ("README.org" "org-default-readme")
                      ("LICENSE" "license-expat")
                      (".gitignore" "gitignore-default-file"))))



(tpl-dumper-dump-simple "/home/nate/tmp.txt" "org-default-file")
(tpl-dumper-dump-simple "/home/nate/tmp.txt" "python-default-file")


;; TODO: Ability to set as executable
;; TODO: Ability to run git init or other commands

(mk-tree tree-py-proj)


(cons 1 '(2 3))

(mk-tree "./" '("1" "2"))

(mk-tree '("12313" (123123)))
(f-join "./" "123" "123")


(f-join "/" "123/" "/123/")
