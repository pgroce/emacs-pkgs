;;; pg-open.el --- Open files with the system viewer

;; Copyright (C) 2021 Phil Groce

;; Author: Phil Groce <pgroce@gmail.com>
;; Version: 0.2
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

(provide 'pg-open)
;;; pg-open.el ends here
