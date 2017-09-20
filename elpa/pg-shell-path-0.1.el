;;; pg-shell-path.el --- Operations on shell paths

;; Copyright (C) 2017 Phil Groce

;; Author: Phil Groce <pgroce@gmail.com>
;; Version: 0.1
;; Keywords: gui

(require 's)

(defun pg/--shell-pathify (pathlist)
  "Convert a list of strings into a shell path specification."
  (if pathlist
      (cl-reduce (lambda (x y): (concat (s-trim x) ":" (s-trim y))) pathlist)
    nil))


(defun pg/--shell-depathify-string (pathspec)
  "Convert a shell path specification PATHSPEC into a list of
strings. If PATHSPEC is nil, returns nil."
  (if pathspec (split-string pathspec ":") nil))


(defun pg/--shell-depathify (pathvar)
  "Convert the value of the environment variable PATHVAR into a
list of strings"
  (pg/--shell-depathify-string (getenv pathvar)))

(defun pg/shell-path-append (pathvar newpaths)
  "Append list of NEWPATHS to the environment variable PATHVAR"
  (let* ((oldpaths (pg/--shell-depathify pathvar))
         (finalpath
          (or (and oldpaths
                   (pg/--shell-pathify (list
                                        (pg/--shell-pathify oldpaths)
                                        (pg/--shell-pathify newpaths))))
              (pg/--shell-pathify newpaths))))
    (setenv pathvar finalpath)))

(defalias 'append-shell-path 'pg/shell-path-append)

(defun pg/shell-path-prepend (pathvar newpaths)
  "Prepend list of NEWPATHS to the environment variable PATHVAR"
  (let* ((oldpaths (pg/--shell-depathify pathvar))
         (finalpath
          (or (and oldpaths
                   (pg/--shell-pathify (list
                                        (pg/--shell-pathify newpaths)
                                        (pg/--shell-pathify oldpaths))))
              (pg/--shell-pathify newpaths))))
    (setenv pathvar finalpath)))

(defalias 'prepend-shell-path 'pg/shell-path-prepend)

(defun pg/shell-path-replace (pathvar newpaths)
  "Replace PATHVAR with NEWPATHS"
  (setenv pathvar (pg/--shell-pathify newpaths)))

(defalias 'replace-shell-path 'pg/shell-path-replace)


(defun pg/shell-path-remove (pathvar path)
  "Remove PATH from PATHVAR"
  (let* ((oldpaths (pg/--shell-depathify pathvar))
         (finalpath (pg/--shell-pathify (remove-if
                                         '(lambda (x) (string= x path))
                                         oldpaths))))
    (setenv pathvar finalpath)))

(defalias 'remove-from-shell-path 'pg/shell-path-remove)

(defun pg/shell-path-spec-as-lines (pathspec)
  "Convert a colon-separated path specification to a series of
lines for display"
  (mapconcat (lambda (x) (concat "    " x))
             (pg/--shell-depathify-string pathspec)
             "\n"))

(defalias 'pathspec-as-lines 'pg/shell-path-spec-as-lines)

(defun pg/shell-path-substitute (pathvar path-a path-b)
  "Replace instances of PATH-A in PATHVAR with PATH-B"
  (interactive "sPATHVAR: \nDDirectory to replace: \nDReplace with: ")
  (let* ((oldpaths (shell-depathify pathvar))
         (repl-fn (lambda (x)
                    (or (and (string= x path-a)
                             path-b)
                        x)))
         (finalpath (shell-pathify (mapcar repl-fn oldpaths))))
    (setenv pathvar finalpath)))

(defalias 'replace-in-shell-path 'pg/shell-path-substitute)

(defun pg/shell-path (arg pathvar newpath)
  "Prepend NEWPATH to PATHVAR. If a prefix argument is specified,
append the directory instead."
  (interactive "p\nsShell path var: \nDDirectory to add: ")
  (if arg
      (append-shell-path pathvar (list (expand-file-name newpath)))
    (prepend-shell-path pathvar (list (expand-file-name newpath)))))

(defalias 'shell-path 'pg/shell-path)

(defun print-shell-path (pathvar)
  "Print PATHVAR as a list to the console."
  (interactive "sPATHVAR: ")
  (message "%s is:\n%s" pathvar (pathspec-as-lines (getenv pathvar))))

(defalias 'print-shell-path 'pg/shell-path-print)

(provide 'pg-shell-path)
;;; pg-shell-path.el ends here
