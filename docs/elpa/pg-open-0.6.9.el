;;; pg-open.el --- Open files with the system viewer

;; Copyright (C) 2021 Phil Groce

;; Author: Phil Groce <pgroce@gmail.com>
;; Version: 0.6.9
;; Keywords: shell

(require 'dired)

(defun pg-open-opener ()
  "Return the general-purpose application opener on this system."
  (cond
   ((eq system-type 'darwin) "open")
   ((or (eq system-type 'windows-nt)
        (eq system-type 'cygwin)) "explorer")
   ;; Assume some kind of unix system
   (t "xdg-open")))

(defun pg-open-thing (thing)
  (shell-command (format "%s \"%s\"" (pg-open-opener) thing)))

 ;;;###autoload
(defun pg-open-dir (dir)
  "Open a directory in the default handler of the current window
system, if one is defined."
  (interactive "DOpen directory (external): ")
  (pg-open-thing dir))

 ;;;###autoload
(defun pg-open-file (f)
  "Open a file in the default handler of the current window
system, if one is defined."
  (interactive "fOpen file (external): ")
  (pg-open-thing f))


 ;;;###autoload
(defun pg-open-file-dired ()
  "Open a file in dired mode using the default handler of the
current window system, if one is defined."
  (interactive)
  (let ((file-list (dired-get-marked-files)))
    (dired-do-shell-command (pg-open-opener) nil file-list)))

(defvar pg-open--org-url-scheme "extfile"
  "URL scheme registered to handle files with `pg-open'. Used by
  `pg-open--link-complete-fn' to build the org-link, and by
  `pg-open-register-org-link' to register the URL type.

This variable is only used internally by those functions;
changing it is not advised. To change the link type used, call
`pg-open-register-org-link' with a different name for the link
type.")

(defun pg-open--link-complete-fn (&optional arg)
  "Create an externally-opened file link using completion."
  ;; (This is just org-link-complete-file with the serial numbers
  ;; filed off.)
  (let ((url-scheme (concat pg-open--org-url-scheme ":"))
        (file (read-file-name "File: "))
        (pwd (file-name-as-directory (expand-file-name ".")))
        (pwd1 (file-name-as-directory (abbreviate-file-name
                                       (expand-file-name ".")))))
    (cond ((equal arg '(16))
           (concat url-scheme
                   (abbreviate-file-name (expand-file-name file))))
          ((string-match
            (concat "^" (regexp-quote pwd1) "\\(.+\\)") file)
           (concat url-scheme (match-string 1 file)))
          ((string-match
            (concat "^" (regexp-quote pwd) "\\(.+\\)")
            (expand-file-name file))
           (concat url-scheme
                   (match-string 1 (expand-file-name file))))
          (t (concat url-scheme file)))))



(declare-function org-link-set-parameters "ol")

;;;###autoload
(defun pg-open-register-org-link (&optional link-type)
  "Define an org link type (i.e., URL scheme) similar to file:,
that uses `pg-open-file' to open the file instead of Emacs's
default facility. This scheme is useful for files like PDF files
that Emacs can read, but that the user may prefer to read with an
external application instead.

LINK-TYPE optionally specifies the name of the link type to be
used. By default, this function will use the type \"extfile\".

This function requires that the org function
`org-link-set-parameters' be defined. This can be ensured by
loading `org-mode' before running this function."

  (when link-type
    (setq pg-open--org-url-scheme link-type))
  (org-link-set-parameters pg-open--org-url-scheme
                           :follow #'pg-open-file
                           :complete #'pg-open--link-complete-fn))

(provide 'pg-open)
;;; pg-open.el ends here
