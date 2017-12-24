;;; pg-password.el --- Password utility functions

;; Copyright (C) 2017 Phil Groce

;; Author: Phil Groce <pgroce@gmail.com>
;; Version: 0.2
;; Keywords: gui

(require 's)

(defun pg/encode-number (number key)
  (format "%X" (logxor key number)))

(defun pg/encode-numbers (numbers key)
  (mapcar (lambda (i) (pg/encode-number i key)) numbers))

(defun pg/decode-number (number key)
  (logxor key (string-to-number number 16)))

(defun pg/decode-numbers (numbers key)
  (mapcar (lambda (i) (pg/decode-number i key)) numbers))

(defcustom pg/password-wordlist-file nil
  "Location of file containing Diceware wordlist.")

(defmacro pg/--with-wordlist (var &rest body)
  "Do BODY with the wordlist, which shall be bound to the name in
VAR."
  (declare (indent defun))
  `(if (file-exists-p pg/password-wordlist-file)
       (let ((,var pg/password-wordlist-file))
         ,@body)
     (message
      "Could not locate '%s'. Is pg/password-wordlist-file set correctly?"
      pg/password-wordlist-file)))

(defun pg/list-from-diceware-entry (entry-string)
  (let ((recs (s-split "[[:space:]]+" (s-trim entry-string))))
    (list (string-to-int (car recs)) (cadr recs))))

(defun pg/word-from-diceware-entry (entry-string)
  (cadr (pg/list-from-diceware-entry entry-string)))

(defun pg/number-from-diceware-entry (entry-string)
  (car (pg/list-from-diceware-entry entry-string)))

;;;###autoload
(defun pg-password-gen (key numwords)
  "Generate a random diceware password. Inserts two lists into
the buffer. The first is a set of numbers corresponding to the
words in the wordlist, XORed with the key; if desired, this may
be used as a password hint. The second is the list of words.

Diceware passphrases of sufficient length have been shown to be
strong passwords, but it is only a little harder (and far
stronger) to use the passphrase as a seed in a personal password
generation algorithm with additional steps, and to change your
passwords often."
  (interactive "nKey: \nnNumber of words: ")
  (pg/--with-wordlist wl
    (let* ((items
            (mapcar #'pg/list-from-diceware-entry
                    (s-lines
                     (s-trim
                      (shell-command-to-string
                       (format
                        "gshuf %s | head -%d" wl numwords))))))
           (numbers-encoded (pg/encode-numbers (mapcar #'car items) key))
           (words (mapcar #'cadr items)))
      (insert (prin1-to-string numbers-encoded))
      (insert "\n")
      (insert (prin1-to-string words)))))

(defun pg/number-to-word (key num)
  "Lookup the diceware word corresponding to NUM, which is a
  number encrypted with KEY."
  ;; Yeah, I know I could do this with elisp, but this is consistent
  ;; with my use of shuf in gen-password, and doesn't require me
  ;; keeping an extra buffer hanging around. Maybe I'll change it
  ;; someday.
  (pg/--with-wordlist wl
    (let ((decoded (pg/decode-number num key)))
      (pg/word-from-diceware-entry
       (shell-command-to-string
        (format
         "grep %d %s" decoded wl))))))

;;;###autoload
(defun pg-password-decrypt-sexp (key)
  "Lookup the list (sexp) of encrypted numbers at point in a
wordlist file, returning the words. KEY is used to decrypt the
numbers."
  (interactive "nKey: ")
  (let ((words (sexp-at-point)))
    (with-output-to-temp-buffer "secretsauce"
      (print
       (if (listp words)
           (mapcar (lambda (w) (pg/number-to-word key w)) words)
         (pg/number-to-word key words))))))

(provide 'pg-password)
;;; pg-password.el ends here
