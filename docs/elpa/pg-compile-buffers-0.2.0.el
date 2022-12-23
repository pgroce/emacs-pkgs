;;; pg-compile-buffers.el --- Toggle reuse of compile buffers

;; Copyright (C) 2017 Phil Groce

;; Author: Phil Groce <pgroce@gmail.com>
;; Version: 0.2.0
;; Keywords: utility

(defun pg-compile-buffers--buffer-per-buffer (mm &optional buff-name)
  "Generate a name for a compiler buffer."
  (let ((real-buffer-name (or buff-name (buffer-name))))
    (if (string-match (concat "*" (downcase mm) " (\\([^)]*\\))\\*")
                      real-buffer-name)
        (concat "*"
                (downcase mm) " (" (match-string 1 real-buffer-name) ")*")
      (concat "*"
              (downcase mm) " (" real-buffer-name ")*"))))



;;;###autoload
(defun pg-compile-buffers-cycle ()
  "Cycle between compilation-mode buffer naming functions."
  (interactive)
  (if  (eq compilation-buffer-name-function
           'pg/compilation-buffer-name-function)
      (progn
        (message "Compilation will use *compilation* buffer")
        (setq compilation-buffer-name-function nil))
    (progn
      (message "Compilation will use per-file buffers")
      (setq compilation-buffer-name-function
            'pg-compile-buffers--buffer-per-buffer)))
  )

(provide 'pg-compile-buffers)
;;; pg-compile-buffers.el ends here
