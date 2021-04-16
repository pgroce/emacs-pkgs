;;; pg-pandoc.el --- Convert document formats with Pandoc

;; Copyright (C) 2020 Phil Groce

;; Author: Phil Groce <pgroce@gmail.com>
;; Version: 0.5
;; Keywords: pandoc markdown org-mode latex pdf docx

(require 'pg-util)
(require 'cl-lib)

(defcustom pg-pandoc-executable-name "pandoc"
  "name (and path, if necessary) of pandoc executable")

(defcustom pg-pandoc-commands
  '(("docx" . "%(pandoc) --citeproc -t docx -f %(in-format) -o %(out-name) --reference-doc=reference.docx")
    ("tex"  . "%(pandoc) --citeproc -t latex -f %(in-format) -o %(out-name) --template=pdf-template.tex")
    ("pdf"  . "%(pandoc) --citeproc -t pdf -f %(in-format) -o %(out-name) --template=pdf-template.tex --pdf-engine xelatex -N")
    ("md"   . "%(pandoc) -t markdown -f %(in-format) -o %(out-name)")
    ("org"  . "%(pandoc) -t org -f %(in-format) -o %(out-name)"))
  "Commands to be used to invoke pandoc on org-mode output using
  `pg-pandoc-subtree'")

(defcustom pg-pandoc-subtree-top-is-title t
  "If set to `t', `pg-pandoc-subtree' will set the top-level headline of the subtree being converted to be the title, the subheads below it to be level 1 headings, etc. This is the default behavior.

If set to `nil', the converted document will contain no title and the top-level headline will be a level 1 headline, its subheads will be level 2 headlines, etc.")


(defcustom pg-pandoc-convert-citations t
  "If set to `t', convert citations (in org-ref format) to
  pandoc-format citations prior to conversion.")

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

(defun pg-pandoc--sanitize-for-file-name (unclean)
  "Downcase UNCLEAN, convert some dividers to hyphens, and
remove everything else"
  (let ((bad-chars-regexp "[^a-z-_,' ]")
        (to-hyphens-regexp "[_,' ]"))
    (replace-regexp-in-string
     to-hyphens-regexp "-"
     (replace-regexp-in-string
      bad-chars-regexp ""
      (downcase unclean)))))

(defun pg-pandoc--buffer-info-to-file-name (output-format)
  (if (buffer-file-name)
      (concat (file-name-base) "." output-format)
    (concat
     (pg-pandoc--sanitize-for-file-name (buffer-name)) "." output-format)))

(defun pg-pandoc--apply-command-template (tplt out-name in-fmt)
  (let ((vars-alist (list (list "out-name" out-name)
                          (list "in-format" in-fmt)
                          (list "pandoc" pg-pandoc-executable-name))))
    (cl-reduce (lambda (acc n)
                 (let ((lbl (car n))
                       (val (cadr n)))
                   (replace-regexp-in-string (format "%%(%s)" lbl) val acc)))
               vars-alist
               :initial-value tplt)))

(defun pg-pandoc--get-command-template (key)
  (alist-get key pg-pandoc-commands nil nil 'equal))

(defun pg-pandoc--convert-citations-in-buffer ()
  "Convert citations in the current buffer from the org-ref
  format to the pandoc format. Currently only supports citep (sort of)."

  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "citep:\\(.+?\\)\\([^a-zA-Z0-9:_-]\\|$\\)" nil t)
      (replace-match "(@\\1)\\2")))
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "cite:\\(.+?\\)\\([^a-zA-Z0-9:_-]\\)" nil t)
      (replace-match "@\\1\\2")))
  )

(defun pg-pandoc--make-top-of-subtree-title ()
  ;; Precondition: point is at the top-level subtree headline
  (let (acc
        (title (save-match-data
                 (looking-at org-complex-heading-regexp)
                 (buffer-substring-no-properties
                  (match-beginning 4) (match-end 4)))))
    ;; TODO: I could try harder here if there is a gap in headline
    ;; level (e.g., a level-1 headline followed by a level-3 headline)
    (org-map-entries (lambda () (add-to-list 'acc (point))) "+LEVEL=2")
    (setq acc (nreverse acc))
    (save-excursion
      (mapc
       (lambda (pos)
         (goto-char pos)
         (org-promote-subtree))
       acc))
    (goto-char (point-min))
    (kill-whole-line)
    (insert (concat "#+TITLE: " title "\n"))))

(defun pg-pandoc-org-subtree (invert-top-is-title)
  (interactive "P")
  (let* ((top-is-title (if invert-top-is-title
                           (not pg-pandoc-subtree-top-is-title)
                         pg-pandoc-subtree-top-is-title))
         (input-format (if (equal "org-mode" (format "%s" major-mode))
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
                 (pg-pandoc--sanitize-for-file-name
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
                    do (org-promote-subtree)))
            ;; Adjust for top-is-title
            ;;
            ;; (Get the position of all the level-1 subheads, promote
            ;; them in reverse order. Can't just (org-map-entries
            ;; #'org-promote-subtree because it will make new level-1
            ;; subheads which we don't want to promote.) )
            (when top-is-title
              (pg-pandoc--make-top-of-subtree-title)))
          (insert "\n")
          ;; debug
          (write-file "debug-out.org")
          (when pg-pandoc-convert-citations
            (message "converting!!!")
            (pg-pandoc--convert-citations-in-buffer))
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
         (fname (pg-pandoc--buffer-info-to-file-name output-format))
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
        (kill-ring-save (point-min) (point-max))
        (with-temp-buffer
          (yank)
          (if pg-pandoc-convert-citations
              (progn
                (message "converting!!!")
                (pg-pandoc--convert-citations-in-buffer))
            (message "NOT CONVERTING!!!"))
          ;; debug
          (write-file "debug-out.org")
          (shell-command-on-region
           (point-min) (point-max)
           pandoc-command
           (get-buffer-create "*pandoc-output*")))))))

(provide 'pg-pandoc)
;;; pg-pandoc.el ends here
