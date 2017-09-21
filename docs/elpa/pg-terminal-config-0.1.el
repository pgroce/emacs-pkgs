;;; pg-terminal-config.el --- Window system configuration

;; Copyright (C) 2017 Phil Groce

;; Author: Phil Groce <pgroce@gmail.com>
;; Version: 0.1
;; Keywords: gui

(defvar pg/t-setup-hook '()
  "Run in term-setup-hook to customize Emacs run on UNIX text
  terminals (including OS X)")
(add-hook 'pg/t-setup-hook (lambda () (message "Running pg/t-setup-hook")))

(defvar pg/x-setup-hook '()
  "Run in term-setup-hook to customize Emacs run on UNIX
  X-Windows (including OS X?)")
(add-hook 'pg/t-setup-hook (lambda () (message "Running pg/x-setup-hook")))

(defvar pg/w32-setup-hook '()
  "Run in term-setup-hook to customize Emacs run on
  Windows (GUI)")
(add-hook 'pg/t-setup-hook (lambda () (message "Running pg/w32-setup-hook")))

(defvar pg/ns-setup-hook '()
  "Run in term-setup-hook to customize Emacs run on OS
  X (GUI)")
(add-hook 'pg/t-setup-hook (lambda () (message "Running pg/ns-setup-hook")))

(defvar pg/pc-setup-hook '()
  "Run in term-setup-hook to customize Emacs run on Windows
  text terminals (so-called \"DOS terminals\".)")
(add-hook 'pg/t-setup-hook (lambda () (message "Running pg/pc-setup-hook")))

(add-hook 'term-setup-hook
          (lambda ()
            (case window-system
              ('t (run-hooks 'pg/t-setup-hook))
              ('x (run-hooks 'pg/x-setup-hook))
              ('w32 (run-hooks 'pg/w32-setup-hook))
              ('ns (run-hooks 'pg/ns-setup-hook))
              ('pc (run-hooks 'pg/pc-setup-hook)))))

(provide 'pg-terminal-config)
;;; pg-terminal-config.el ends here
