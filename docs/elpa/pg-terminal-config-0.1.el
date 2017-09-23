;;; pg-terminal-config.el --- Window system configuration

;; Copyright (C) 2017 Phil Groce

;; Author: Phil Groce <pgroce@gmail.com>
;; Version: 0.1
;; Keywords: gui

(defcustom pg-terminal-t-setup-hook '()
  "Run in term-setup-hook to customize Emacs run on UNIX text
  terminals (including OS X)")
(defcustom pg-terminal-x-setup-hook '()
  "Run in term-setup-hook to customize Emacs run on UNIX
  X-Windows (including OS X?)")
(defcustom pg-terminal-w32-setup-hook '()
  "Run in term-setup-hook to customize Emacs run on
  Windows (GUI)")
(defcustom pg-terminal-ns-setup-hook '()
  "Run in term-setup-hook to customize Emacs run on OS
  X (GUI)")
(defcustom pg-terminal-pc-setup-hook '()
  "Run in term-setup-hook to customize Emacs run on Windows
  text terminals (so-called \"DOS terminals\".)")

;;;###autoload
(defun pg-terminal-setup-hooks ()
  (add-hook 'pg-terminal-t-setup-hook
            (lambda () (message "Running pg-terminal-t-setup-hook")))
  (add-hook 'pg-terminal-t-setup-hook
            (lambda () (message "Running pg-terminal-x-setup-hook")))
  (add-hook 'pg-terminal-t-setup-hook
            (lambda () (message "Running pg-terminal-w32-setup-hook")))
  (add-hook 'pg-terminal-t-setup-hook
            (lambda () (message "Running pg-terminal-ns-setup-hook")))
  (add-hook 'pg-terminal-t-setup-hook
            (lambda () (message "Running pg-terminal-pc-setup-hook")))
  (add-hook 'term-setup-hook
            (lambda ()
              (case window-system
                ('t (run-hooks 'pg-terminal-t-setup-hook))
                ('x (run-hooks 'pg-terminal-x-setup-hook))
                ('w32 (run-hooks 'pg-terminal-w32-setup-hook))
                ('ns (run-hooks 'pg-terminal-ns-setup-hook))
                ('pc (run-hooks 'pg-terminal-pc-setup-hook))))))

(provide 'pg-terminal-config)
;;; pg-terminal-config.el ends here
