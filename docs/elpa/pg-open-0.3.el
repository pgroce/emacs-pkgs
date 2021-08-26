;;; pg-open.el --- Open files with the system viewer

;; Copyright (C) 2021 Phil Groce

;; Author: Phil Groce <pgroce@gmail.com>
;; Version: 0.3
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

(defcustom pg-open-org-url-scheme "extfile"
  "String defining the url scheme of custom Org mode
  URL. Note: Changing this value will not change the way URLs are
  opened/selected in Org mode unless the corresponding link type
  parameters are set/updated in `org-link-set-parameters'.")

;; Define an extfile: url type that opens with the OS file opener
;; (using pg-open). (This is just org-link-complete-file with the
;; serial numbers filed off.)
(defun pg-open-link-complete-fn (&optional arg)
  "Create an externally-opened file link using completion."
  (let ((url-scheme (concat pg-open-org-url-scheme ":"))
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

(provide 'pg-open)
;;; pg-open.el ends here
