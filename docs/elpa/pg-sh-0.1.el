;;; pg-sh.el --- Manage local and remote shells

;; Copyright (C) 2017 Phil Groce

;; Author: Phil Groce <pgroce@gmail.com>
;; Version: 0.1
;; Package-Requires: (tramp)
;; Keywords: shell

(require 'tramp)

;;; buff can be nil, in which case the default is the current buffer
(defun pg-sh--make-or-destroy-temporary-shell (buffname def-dir)
  (if (string-match-p "^\\*temp-shell (" (buffer-name))
      (let ((kill-buffer-query-functions '()))
        (kill-buffer))
    (let ((default-directory (if def-dir
                                 (if (string-match-p "/$" def-dir)
                                     def-dir
                                   (concat def-dir "/"))
                               default-directory)))
      (shell (concat "*temp-shell (" buffname ")*")))))

;;;###autoload
(defun pg-sh ()
  "Create a local shell buffer, or dismiss an active one.

If the current buffer is a shell buffer previously created with
`pg-sh-local', destroy it. If it's not, either create a new
shell buffer associated with this buffer, or jump to one if it
already exists."
  (interactive)
  (pg-sh--make-or-destroy-temporary-shell (buffer-name) nil))

(defun pg-sh--ssh-hosts ()
  "Return all the hosts configured in ~/.ssh/config via
  tramp-parse-sconfig."
  ;; tramp-parse-sconfig returns a list of tuples whose cars are
  ;; always nil. The cdr is often nil, too, so the thing ends up
  ;; looking like:
  ;;
  ;;  '(nil nil nil (nil "foo") nil (nil "bar")...)
  ;;
  ;; This function, given that, would return: '("foo" "bar")
  (let ((not-nil-p (lambda (x) (not (equal x nil)))))
    (mapcar
     'car
     (cl-remove-if-not
      not-nil-p
      (mapcar 'cdr (tramp-parse-sconfig "~/.ssh/config"))))))

(defun pg-sh--make-dumb-remote-terminal (remote-host)
  "Make a remote connection via ssh. REMOTE-HOST is presumed to
  be sufficient to `ssh' to figure things out."
  (let* ((bdir (format "ssh:%s" remote-host))
         (bname (generate-new-buffer-name bdir))
         (buff (make-term bname "ssh" nil remote-host)))
    (set-buffer buff)
    (setq default-directory (format "/%s:" bdir))
    (term-mode)
    (term-char-mode)
    (term-set-escape-char ?\C-x)
    (switch-to-buffer buff)))

;;;###autoload
(defun pg-sh-ssh ()
  (interactive)
  "Make a remote terminal from an SSH configuration entry. The
  entry name is presumed to be sufficient to initiate the SSH
  connection."
  (pg-sh--make-dumb-remote-terminal
   (completing-read "SSH config entry: " (pg-sh--ssh-hosts))))

(provide 'pg-sh)
;;; pg-sh.el ends here
