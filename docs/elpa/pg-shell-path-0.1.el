;;; pg-shell-path.el --- Operations on shell paths

;; Copyright (C) 2017 Phil Groce

;; Author: Phil Groce <pgroce@gmail.com>
;; Version: 0.1
;; Keywords: gui
;; Package-Requires: (s)

(require 's)

(defun pg-shell-path--pathify (pathlist)
  "Convert a list of strings into a shell path specification."
  (if pathlist
      (cl-reduce (lambda (x y): (concat (s-trim x) ":" (s-trim y))) pathlist)
    nil))


(defun pg-shell-path--depathify-string (pathspec)
  "Convert a shell path specification PATHSPEC into a list of
strings. If PATHSPEC is nil, returns nil."
  (if pathspec (split-string pathspec ":") nil))


(defun pg-shell-path--depathify (pathvar)
  "Convert the value of the environment variable PATHVAR into a
list of strings"
  (pg-shell-path--depathify-string (getenv pathvar)))

;;;###autoload
(defun pg-shell-path-append (pathvar newpaths)
  "Append list of NEWPATHS to the environment variable PATHVAR"
  (let* ((oldpaths (pg-shell-path--depathify pathvar))
         (finalpath
          (or (and oldpaths
                   (pg-shell-path--pathify
                    (list
                     (pg-shell-path--pathify oldpaths)
                     (pg-shell-path--pathify newpaths))))
              (pg-shell-path--pathify newpaths))))
    (setenv pathvar finalpath)))

;;;###autoload
(defun pg-shell-path-contains-p (pathvar item)
  "Returns a true value if ITEM is not in PATHVAR, else nil"
  (member item (pg-shell-path--depathify pathvar)))

;;;###autoload
(defun pg-shell-path-prepend (pathvar newpaths)
  "Prepend list of NEWPATHS to the environment variable PATHVAR"
  (let* ((oldpaths (pg-shell-path--depathify pathvar))
         (finalpath
          (or (and oldpaths
                   (pg-shell-path--pathify
                    (list
                     (pg-shell-path--pathify newpaths)
                     (pg-shell-path--pathify oldpaths))))
              (pg-shell-path--pathify newpaths))))
    (setenv pathvar finalpath)))


;;;###autoload
(defun pg-shell-path-replace (pathvar newpaths)
  "Replace PATHVAR with NEWPATHS"
  (setenv pathvar (pg-shell-path--pathify newpaths)))

;;;###autoload
(defun pg-shell-path-remove (pathvar path)
  "Remove PATH from PATHVAR"
  (let* ((oldpaths (pg-shell-path--depathify pathvar))
         (finalpath (pg-shell-path--pathify
                     (remove-if
                      '(lambda (x) (string= x path))
                      oldpaths))))
    (setenv pathvar finalpath)))

;;;###autoload
(defun pg-shell-path-spec-as-lines (pathspec)
  "Convert a colon-separated path specification to a series of
lines for display"
  (mapconcat (lambda (x) (concat "    " x))
             (pg-shell-path--depathify-string pathspec)
             "\n"))

(defun pg-shell-path-substitute (pathvar path-a path-b)
  "Replace instances of PATH-A in PATHVAR with PATH-B"
  (interactive "sPATHVAR: \nDDirectory to replace: \nDReplace with: ")
  (let* ((oldpaths (shell-depathify pathvar))
         (repl-fn (lambda (x)
                    (or (and (string= x path-a)
                             path-b)
                        x)))
         (finalpath (shell-pathify (mapcar repl-fn oldpaths))))
    (setenv pathvar finalpath)))

(defalias 'replace-in-shell-path 'pg-shell-path-substitute)

(defun pg-shell-path (arg pathvar newpath)
  "Prepend NEWPATH to PATHVAR. If a prefix argument is specified,
append the directory instead."
  (interactive "p\nsShell path var: \nDDirectory to add: ")
  (if arg
      (pg-shell-path-append pathvar (list (expand-file-name newpath)))
    (pg-shell-path-prepend pathvar (list (expand-file-name newpath)))))

(defun pg-shell-path-print (pathvar)
  "Print PATHVAR as a list to the console."
  (interactive "sPATHVAR: ")
  (message "%s is:\n%s" pathvar (pathspec-as-lines (getenv pathvar))))

(provide 'pg-shell-path)
;;; pg-shell-path.el ends here
