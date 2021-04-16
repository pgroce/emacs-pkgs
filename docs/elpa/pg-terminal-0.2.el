;;; pg-terminal.el --- Window system configuration

;; Copyright (C) 2017 Phil Groce

;; Author: Phil Groce <pgroce@gmail.com>
;; Version: 0.2
;; Keywords: gui

(defmacro pg-terminal-t (&rest body)
  "Arrange for BODY to be executed after terminal setup if the window-system is `t` (TTY terminal)"
  `(when (eq (window-system) 't)
      (add-hook 'emacs-startup-hook (lambda () ,@body))))

(defmacro pg-terminal-x (&rest body)
  "Arrange for BODY to be executed after terminal setup if the window-system is `x` (X Windows GUI)"
  `(when (eq (window-system) 'x)
      (add-hook 'emacs-startup-hook (lambda () ,@body))))

(defmacro pg-terminal-w32 (&rest body)
  "Arrange for BODY to be executed after terminal setup if the window-system is `w32` (MS Windows)"
  `(when (eq (window-system) 'w32)
      (add-hook 'emacs-startup-hook (lambda () ,@body))))

(defmacro pg-terminal-ns (&rest body)
  "Arrange for BODY to be executed after terminal setup if the window-system is `ns` (NeXTStep/OS X)"
  `(when (eq (window-system) 'ns)
      (add-hook 'emacs-startup-hook (lambda () ,@body))))

(defmacro pg-terminal-pc (&rest body)
  "Arrange for BODY to be executed after terminal setup if the window-system is `pc` (DOS console)"
  `(when (eq (window-system) 'pc)
     (add-hook 'emacs-startup-hook (lambda () ,@body))))

(defmacro pg-terminal-any (&rest body)
  "Arrange for BODY to be executed after terminal setup if the window-system is `pc` (DOS console)"
  `(add-hook 'emacs-startup-hook (lambda () ,@body)))

(provide 'pg-terminal)
;;; pg-terminal.el ends here
