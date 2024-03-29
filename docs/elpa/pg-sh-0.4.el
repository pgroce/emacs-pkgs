;;; pg-sh.el --- Manage local and remote shells

;; Copyright (C) 2017 Phil Groce

;; Author: Phil Groce <pgroce@gmail.com>
;; Version: 0.4
;; Keywords: shell

(require 'tramp)
(require 'cl-lib)

;;;###autoload
(defun pg-sh (&optional context def-dir)
  "Make/destroy a temporary shell, as appropriate.

Called with no arguments, this function will kill a temp-shell
buffer if one is the current active buffer; otherwise, it will
create one associated with the current active buffer, in that
buffer's default directory.

CONTEXT is the context with which the shell will be
associated; by default, it is the name of the current active
buffer.  If CONTEXT is the name of an existing temporary shell
buffer, destroy it.  Otherwise, start a new temporary shell
buffer associated with that context.

If DEF-DIR is supplied, make that the shell's working directory."
  (interactive)
  (let ((context (if context context (buffer-name))))
    ;; Don't allow the creation of a temp-shell if the associated
    ;; buffer doesn't exist. It can go away later, but requiring it to
    ;; exist now makes things simpler (i.e., we know kill-buffer
    ;; will work).

    ;; If context is the name of a temp-shell buffer, kill it (if it
    ;; exists)
    (if (string-match-p "^\\*temp-shell (" context)
        (if (get-buffer context)
            (let ((kill-buffer-query-functions '()))
              (kill-buffer))
          (message "Refusing to kill nonexistent temp-shell '%s'" context))
      ;; Otherwise, create a temp-shell for it
      (let ((default-directory (if def-dir
                                   (if (string-match-p "/$" def-dir)
                                       def-dir
                                     (concat def-dir "/"))
                                 default-directory)))
        (shell (concat "*temp-shell (" context ")*"))))))

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

(defun pg-sh--make-dumb-terminal (buff-name default-dir program &rest switches)
  (let* ((buff (apply #'make-term buff-name program nil args)))
    (set-buffer buff)
    (setq default-directory default-dir)
    (term-mode)
    (term-char-mode)
    (term-set-escape-char ?\C-x)
    (switch-to-buffer buff)))

(defun pg-sh--make-dumb-remote-terminal (remote-host)
  "Make a remote connection via ssh. REMOTE-HOST is presumed to
  be sufficient to `ssh' to figure things out."
  (let* ((bdir (format "ssh:%s" remote-host))
         (bname (generate-new-buffer-name bdir)))
    (pg-sh--make-dumb-remote-terminal bname (format "/%s:" bdir) "ssh" remote-host)))

;;;###autoload
(defun pg-sh-ssh ()
  (interactive)
  "Make a remote terminal from an SSH configuration entry. The
  entry name is presumed to be sufficient to initiate the SSH
  connection."
  (pg-sh--make-dumb-remote-terminal
   (completing-read "SSH config entry: " (pg-sh--ssh-hosts))))

;; For reference....
(defun pg-sh--make-dumb-terminal (buff-name default-dir program &rest switches)
  (let* ((buff (apply #'make-term buff-name program nil args)))
    (set-buffer buff)
    (setq default-directory default-dir)
    (term-mode)
    (term-char-mode)
    (term-set-escape-char ?\C-x)
    (switch-to-buffer buff)))

;;;###autoload
(defun pg-sh-ansi-term (&optional context default-dir program &rest program-switches)
  "Make/destroy a temporary terminal emulator, as appropriate.
The semantics for this function are identical to `pg-shell', but
start a terminal emulator (using `term.el') instead of a raw
shell (using `shell'). Also, the program to be run will be read
from the minibuffer, so programs other than shells can be run.

The logic for choosing the default program to run is identical to
that in `ansi-term': use the value of `explicit-shell-file-name',
the value of the `ESHELL' environment variable if that isn't set,
and if neither are set, the value of `shell-file-name'. Arguments
may be supplied in addition to the program name.

If `PROGRAM' is specified, it will be run with `PROGRAM-SWITCHES'
as arguments, and the user will not be prompted to enter a command."
  (interactive)
  (let ((context (if context context (buffer-name))))
    ;; If context is the name of an existing buffer, kill it
    (if (string-match-p "^\\*temp-term (" context)
        (if (get-buffer context)
            (let ((kill-buffer-query-functions '()))
              (kill-buffer))
          (message "Refusing to kill nonexistent temp-term '%s'" context))
      ;; Otherwise, create a new terminal
      (let ((default-dir (if default-dir
                             (if (string-match-p "/$" default-dir)
                                 default-dir
                               (concat default-dir "/"))
                           default-directory))
            (new-context (concat "*temp-term (" context ")*"))
            (tokens (if program
                        (cons program program-switches)
                      (split-string " " (read-from-minibuffer "Run program: "
                                                              (or explicit-shell-file-name
                                                                  (getenv "ESHELL")
                                                                  shell-file-name))))))
        (if (car tokens)
            (pg-sh--make-dumb-terminal context default-dir (car tokens) (cdr tokens))
          (message "No program specified, doing nothing"))))))

(provide 'pg-sh)
;;; pg-sh.el ends here
