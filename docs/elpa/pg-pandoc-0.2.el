;;; pg-pandoc.el --- Convert document formats with Pandoc

;; Copyright (C) 2020 Phil Groce

;; Author: Phil Groce <pgroce@gmail.com>
;; Version: 0.2
;; Keywords: pandoc markdown org-mode latex pdf docx

(require 'pg-util)
(require 'cl-lib)

(defcustom pg-pandoc-commands
  '(("docx" . "pandoc -t docx -f %(in-format) -o %(out-name) --reference-doc=reference.docx")
    ("tex" . "pandoc -t latex -f %(in-format) -o %(out-name) --template=pdf-template.tex")
    ("pdf" . "pandoc -t pdf -f %(in-format) -o %(out-name) --template=pdf-template.tex --pdf-engine xelatex -N"))
  "Commands to be used to invoke pandoc on org-mode output using
  `pg-pandoc-subtree'")

(defcustom pg-pandoc--pandoc-command-history nil
  "History variable for use with `pg-pandoc-org-subtree' and
  `pg-pandoc-org'")



(defun pg-pandoc--org-headline ()
  "Return the headline level (i.e., number of stars) and title of
the headline."
  (save-excursion
    (save-match-data
      (org-back-to-heading)
      (let ((case-fold-search nil))
        (looking-at org-complex-heading-regexp)
        (list (length (buffer-substring-no-properties
                       (match-beginning 1)
                       (match-end 1)))
              (buffer-substring-no-properties
               (match-beginning 4)
               (match-end 4)))))))

(defun pg-pandoc--headline-to-file-name (headline)
  "Downcase HEADLINE, convert some dividers to hyphens, and
remove everything else"
  (let ((bad-chars-regexp "[^a-z-_,' ]")
        (to-hyphens-regexp "[_,' ]"))
    (replace-regexp-in-string
     to-hyphens-regexp "-"
     (replace-regexp-in-string
      bad-chars-regexp ""
      (downcase headline)))))

(defun pg-pandoc--apply-command-template (tplt out-name in-fmt)
  (let ((vars-alist (list (list "out-name" out-name)
                          (list "in-format" in-fmt))))
    (cl-reduce (lambda (acc n)
                 (let ((lbl (car n))
                       (val (cadr n)))
                   (replace-regexp-in-string (format "%%(%s)" lbl) val acc)))
               vars-alist
               :initial-value tplt)))

(defun pg-pandoc--get-command-template (key)
  (alist-get key pg-pandoc-commands nil nil 'equal))

(defun pg-pandoc-org-subtree ()
  (interactive)
  (let* ((input-format (if (equal "org-mode" (format "%s" major-mode))
                           "org"
                         (error "pg-pandoc-org-subtree only works on org-mode files")))
         (headline (pg-pandoc--org-headline))
         (headline-level (car headline))
         (headline-title (cadr headline))
         ;; Ask user for desired output format
         (output-format (completing-read "Output format: "
                                         (pg-util-alist-keys
                                          pg-pandoc-commands)))
         ;; Compute (default) filename from output format
         (fname (concat
                 (pg-pandoc--headline-to-file-name
                  headline-title)
                         "." output-format))
         ;; Ask user for pandoc command (with default)
         (default-command (pg-pandoc--apply-command-template
                           (pg-pandoc--get-command-template output-format)
                           fname input-format))
         (pandoc-command (read-string
                          "Command: "
                          default-command pg-pandoc--pandoc-command-history
                          default-command)))
    (save-excursion
      (save-restriction
        (org-narrow-to-subtree)
        (goto-char (point-min))
        (kill-ring-save (point-min) (point-max))
        (with-temp-buffer
          (org-mode)
          (yank)
          (save-excursion
            (exchange-point-and-mark)
            ;; promote subtree to top level
            (let ((cur-level (org-current-level)))
              (loop repeat (/ (- cur-level 1)
                              (org-level-increment))
                    do (org-promote-subtree))))
          (insert "\n")
          ;; debug
          ;; (write-file "debug-out.org")
          (shell-command-on-region
           (point-min) (point-max)
           pandoc-command
           (get-buffer-create "*pandoc-output*")))))))

(defun pg-pandoc-current-buffer ()
  (interactive)
  (let* ((input-format (replace-regexp-in-string
                        "-mode$" "" (format "%s" major-mode)))
         ;; Ask user for desired output format
         (output-format (completing-read "Output format: "
                                         (pg-util-alist-keys
                                          pg-pandoc-commands)))
         ;; Compute (default) filename from output format
         (fname (concat (file-name-base) "." output-format))
         ;; Ask user for pandoc command (with default)
         (default-command (pg-pandoc--apply-command-template
                           (pg-pandoc--get-command-template output-format)
                           fname input-format))
         (pandoc-command (read-string
                          "Command: "
                          default-command pg-pandoc--pandoc-command-history
                          default-command)))
    (save-excursion
      (save-restriction
        (shell-command-on-region
         (point-min) (point-max)
         pandoc-command
         (get-buffer-create "*pandoc-output*"))))))

(provide 'pg-pandoc)
;;; pg-pandoc.el ends here
