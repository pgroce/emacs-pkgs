;;; pg-url-armor.el --- Set off URLs in text

;; Copyright (C) 2017 Phil Groce

;; Author: Phil Groce <pgroce@gmail.com>
;; Version: 0.1

(require 'thingatpt)

;;;###autoload
(defun pg/armor-url-at-point ()
  "Add angle brackets to the beginning and end of the URL at
  point."
  (interactive)
  (let ((bounds (bounds-of-thing-at-point 'url)))
    (when bounds
      (let* ((beg (car bounds))
             (end (cdr bounds)))
        (save-excursion
          (when (or (not (equal (char-to-string (char-before beg)) "<"))
                    (not (equal (char-to-string (char-after end)) ">")))
            (goto-char beg)
            (kill-new (buffer-substring beg end))
            (delete-region beg end)
            (insert (format "<%s>" (current-kill 0 t)))))))))

;;;###autoload
(defun pg/yank-armored ()
  "Yank a URI or email (presumably, but it could be anything) and
  enclose it in angle brackets."
  (interactive)
  (insert "<")
  (yank)
  (insert ">"))

(provide 'pg-url-armor)
;;; pg-url-armor.el ends here
