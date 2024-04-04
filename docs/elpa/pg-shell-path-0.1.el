;;; pg-shell-path.el --- Operations on shell paths

;; Copyright (C) 2017 Phil Groce

;; Author: Phil Groce <pgroce@gmail.com>
;; Version: 0.1
;; Keywords: gui
;; Package-Requires: (s dash)

(require 's)
(require 'dash)
(require 'cl-lib)

(defun pg-shell-path--pathify (pathlist)
  "Convert a list of strings into a shell path specification."
  (cl-assert (listp pathlist))
  (if (equal pathlist nil)
      ""
    (--reduce (concat acc ":" (s-trim it)) pathlist)))


(defun pg-shell-path--depathify-string (pathspec)
  "Convert a shell path specification PATHSPEC into a list of
strings. If PATHSPEC is an empty string or whitespace, returns
nil."
  (cl-assert (stringp pathspec))
  (let ((pathspec (s-trim pathspec)))
    (if (s-present? pathspec)
        (split-string pathspec ":")
      nil)))


(defun pg-shell-path--depathify (pathvar)
  "Convert the value of the environment variable PATHVAR into a
list of strings"
  (pg-shell-path--depathify-string (getenv pathvar)))

(cl-defmacro pg-shell-path-with-string ((str &key as into) &rest body)
  "Execute body in the context of a depathified shell path.

STR is a string formatted as a shell path. (I.e., it is a string
containing substrings delimited by colons.) It is turned into a
list of strings and made available as the symbol named by AS. (If
AS is not supplied, it is provided via the symbol \"it\".)

BODY will be executed, and should return a list of strings. These
will be converted back into a colon-delimited shell path, which
will be the return value of the expression. If INTO is supplied,
it refers to the name of an environment variable; the result of
this expression will also be written to it."
  (declare (indent defun))
  (let ((-into (make-symbol "-into")))
    `(let* ((,-into ,into)
            (,(if as as 'it) (pg-shell-path--depathify-string ,str))
            (result (pg-shell-path--pathify
                     (progn ,@body))))
       (when (stringp ,-into)
         (setenv ,-into result))
       result)))

(cl-defmacro pg-shell-path-with ((varname &key as into) &rest body)
  "Execute body in the context of a depathified path environment variable.

VARNAME is an environment variable containing a shell path. Its
value will be retrieved from the environment, turned into a list
of path elements, and bound to the symbol specified by AS. (If AS
is not specified, it is bound to the symbol \"it\".)

BODY is executed, and should return a list of strings. This list
will be converted back into a colon-delimited shell path and
returned to the caller. If INTO is supplied, it refers to the
name of an environment variable; the result of this expression
will also be written to it."
  (declare (indent defun))
  `(pg-shell-path-with-string ((getenv ,varname)
                               :as ,as
                               :into ,(if (equal into t)
                                          varname
                                        into))
     ,@body))

;;;###autoload
(defun pg-shell-path-append (pathvar newpaths)
  "Append to environment variable PATHVAR the contents of NEWPATHS.

NEWPATHS can be a list of paths or a string (which is understood to be a single element to add to the path)."
  (let* ((newpaths (if (stringp newpaths) (list newpaths) newpaths))
         (oldpaths (pg-shell-path--depathify pathvar))
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
