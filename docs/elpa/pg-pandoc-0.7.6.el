;;; pg-pandoc.el --- Convert document formats with Pandoc

;; Copyright (C) 2020 Phil Groce

;; Author: Phil Groce <pgroce@gmail.com>
;; Version: 0.7.6
;; Package-Requires: ((emacs "26.1") (dash "2.19") (f "20220608.943"))
;; Keywords: pandoc markdown org-mode latex pdf docx

(require 'pg-util)
(require 'cl-lib)
(require 'dash)
(require 'f)

(defcustom pg-pandoc-executable-name "pandoc"
  "name (and path, if necessary) of pandoc executable")

(defcustom pg-pandoc-commands
  '(("docx" .
     ("docx" "docx" "reference-doc"
      "%(pandoc) --citeproc -t docx -f %(in-format) -o %(out-name) %(refdoc-clause)"))
    ("pptx" .
     ("pptx" "pptx" "reference-doc"
      "%(pandoc) --citeproc -t pptx -V classoption=notes -f %(in-format) -o %(out-name) %(refdoc-clause)"))
    ("tex" .
     ("tex" "tex" "template"
      "%(pandoc) --citeproc -t latex -f %(in-format) -o %(out-name) %(refdoc-clause)"))
    ("beamer" .
     ("pdf" "pdf" "template"
      "%(pandoc) --citeproc -t beamer -f %(in-format) -o %(out-name) %(refdoc-clause)"))
    ("pdf" .
     ("pdf" "tex" "template"
      "%(pandoc) --citeproc -t pdf -f %(in-format) -o %(out-name) %(refdoc-clause) --pdf-engine xelatex -N"))
    ("md" .
     ("md" "" nil
      "%(pandoc) -t markdown -f %(in-format) -o %(out-name)"))
    ("org" .
     ("org" "" nil
      "%(pandoc) -t org -f %(in-format) -o %(out-name)")))
  "Metadata about pandoc output formats. Key is output format,
value is a list of elements. Elements in order:

The first element (0) is the extension of output file

The second element (1) is the extension for the Pandoc reference
document/template, or the empty string if conversion doesn't support a
reference document.

The third element (2) refers to the option pandoc uses to pass
the reference document or template, as a string. This is usually
either \"reference-doc\" or \"template\". If the output format
doesn't support a reference document, this value should be `nil'.

The fourth element (3) is the pandoc command template.

There is no element for input format, as this is variable and
inferred from the input.")

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

(defun pg-pandoc--apply-template (tplt vars-alist)
  "Apply VARS-ALIST to TPLT to generate a new string. VARS-ALIST
  is an alist of parameter names and their string values. The
  values are indicated as such in TPLT by surrouding them with
  parentheses and prepending a percent sign. So the key \"foo\"
  in a template would be referenced as \"%(foo)\"."
  (cl-reduce (lambda (acc n)
               (let ((lbl (car n))
                     (val (cadr n)))
                 (replace-regexp-in-string (format "%%(%s)" lbl) val acc)))
             vars-alist
             :initial-value tplt))

(defun pg-pandoc--convert-citations-in-buffer ()
  "Convert citations in the current buffer from the org-ref
  format to the pandoc format. Currently only supports citep (sort of)."

  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "citep:\\(.+?\\)\\([^a-zA-Z0-9:_-]\\|$\\)" nil t)
      (replace-match "([@\\1;])\\2")))
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "cite:\\(.+?\\)\\([^a-zA-Z0-9:_-]\\)" nil t)
      (replace-match "[@\\1;]\\2")))
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

(defun pg-pandoc--reference-doc-clause (ext reference-option)
  "Ask user for a pandoc reference document for the format
 EXT.

Pandoc uses different names for the reference doc/template
parameter, depending on the output format; REFERENCE-OPTION is
this parameter name, such as \"reference-doc\" or
\"template\". It should be `nil' if the output format does not
support templating.

If REFERENCE-OPTION is `nil' or no candidate is selected, return
an empty string."
  ;; NOTE: Only implemented for docx right now, but theoretically this
  ;; could appy to latex too....
  (if reference-option
      (let* ((cand (read-file-name "Reference doc: " nil nil 'confirm nil
                                   (lambda (f) (or (f-directory? f) (f-ext? f ext))))))
        (if (f-directory? cand)
            ""
          (format "--%s=\"%s\" " reference-option cand)))
    ""))





(defun pg-pandoc--build-command (input-format fname-base)
  "Build the pandoc command from user input and
arguments. INPUT-FORMAT is the input format, FNAME-BASE is the
output file name without extension. (Extension will be based on output format.)"
  (let* (
         (output-format (completing-read "Output format: "
                                         (pg-util-alist-keys
                                          pg-pandoc-commands)))
         (ext (nth 0 (alist-get output-format pg-pandoc-commands nil nil 'equal)))
         (reference-ext (nth 1 (alist-get output-format pg-pandoc-commands nil nil 'equal)))
         (reference-option (nth 2 (alist-get output-format pg-pandoc-commands nil nil 'equal)))
         (template (nth 3 (alist-get output-format pg-pandoc-commands nil nil 'equal)))
         (refdoc-clause (pg-pandoc--reference-doc-clause (pg-util-spy reference-ext) (pg-util-spy reference-option)))
         (out-name (concat "\"" (read-file-name "Output file: " nil nil nil (concat fname-base "." ext)) "\""))
         (default-command (pg-pandoc--apply-template
                           template
                           `(("out-name" ,out-name)
                             ("in-format" ,input-format)
                             ("refdoc-clause" ,refdoc-clause)
                             ("pandoc" ,pg-pandoc-executable-name)))))
    (read-string
     "Command: "
     default-command pg-pandoc--pandoc-command-history
     default-command)))



(defun pg-pandoc-org-subtree (invert-top-is-title)
  "Convert the current org-mode subtree to another output format using Pandoc."
  (interactive "P")
  (let* ((top-is-title (if invert-top-is-title
                           (not pg-pandoc-subtree-top-is-title)
                         pg-pandoc-subtree-top-is-title))
         (input-format (if (equal "org-mode" (format "%s" major-mode))
                           "org"
                         (error "pg-pandoc-org-subtree only works on org-mode files")))

         (headline-title (cadr (pg-pandoc--org-headline)))
         (fname-base (pg-pandoc--sanitize-for-file-name headline-title))
         (pandoc-command (pg-pandoc--build-command input-format fname-base)))
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
              (cl-loop repeat (/ (- cur-level 1)
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
            (pg-pandoc--convert-citations-in-buffer))
          (shell-command-on-region
           (point-min) (point-max)
           pandoc-command
           (get-buffer-create "*pandoc-output*")))))))



(defun pg-pandoc-current-buffer ()
  "Convert the current buffer to another output format using Pandoc.

The input format of the buffer is inferred from the buffer mode."
  (interactive)
  (let* ((input-format (replace-regexp-in-string
                        "-mode$" "" (format "%s" major-mode)))
         (fname-base (if (buffer-file-name)
                         (file-name-base (buffer-file-name))
                       (pg-pandoc--sanitize-for-file-name (buffer-name))))
         (pandoc-command (pg-pandoc--build-command input-format fname-base)))
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
