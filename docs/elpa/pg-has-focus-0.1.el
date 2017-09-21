;;; pg-has-focus.el --- Detect focus changes

;; Copyright (C) 2017 Phil Groce

;; Author: Phil Groce <pgroce@gmail.com>
;; Version: 0.1
;; Keywords: gui

(setq pg/--emacs-has-focus t)
(defun pg/emacs-has-focus-p ()
  "Returns true if Emacs is the selected app by the OS."
  pg/--emacs-has-focus)

(defun pg/update-focus-in () (setq pg/--emacs-has-focus t))
(defun pg/update-focus-out () (setq pg/--emacs-has-focus nil))

(add-hook 'focus-out-hook 'pg/update-focus-out)
(add-hook 'focus-in-hook 'pg/update-focus-in)

(provide 'pg-has-focus)
;;; pg-has-focus.el ends here
