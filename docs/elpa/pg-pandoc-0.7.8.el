;;; pg-pandoc.el --- Convert document formats with Pandoc

;; Copyright (C) 2020 Phil Groce

;; Author: Phil Groce <pgroce@gmail.com>
;; Version: 0.7.8
;; Package-Requires: ((emacs "26.1") (dash "2.19") (f "20220608.943") (pg-org "0.4.7") (pg-util "0.3.4"))
;; Keywords: pandoc markdown org-mode latex pdf docx

(require 'pg-util)
(require 'pg-org)
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
     ("pdf" "tex" "template"
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

(defcustom pg-pandoc-downcase-default-filenames nil
  "If non-`nil', downcase default output file names. By default,
preserve case in default file names.")

(defcustom pg-pandoc-bad-chars-regexp "[^a-zA-Z0-9._-]"
  "Regexp matching illegal characters in an output filename. By
default, only the POSIX \"fully portable filenames\" characters
are used. (See
https://en.wikipedia.org/wiki/Filename#Comparison_of_filename_limitations)")

(defcustom pg-pandoc-allow-leading-hyphens nil
  "If non-`nil', allow an output filename to begin with a
hyphen. Leading hyphens are not permitted in fully POSIX-portable
filenames.")

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

(defun pg-pandoc--maybe-downcase (str)
  "Downcase STR according to the value of `pg-pandoc-downcase-default-filenames'."
  (if pg-pandoc-downcase-default-filenames
      (downcase str)
    str))

(defun pg-pandoc--maybe-remove-leading-hyphens (str)
  "Strip leading hyphens according to the value of `pg-pandoc-allow-leading-hyphens'."
  (if (not pg-pandoc-allow-leading-hyphens)
      (replace-regexp-in-string "^-*" "" str)))

(defun pg-pandoc--sanitize-for-file-name (unclean)
  "Downcase UNCLEAN, convert some dividers to hyphens, and
remove everything else"
  (let ((bad-chars-regexp "[^a-zA-Z0-9._-]")
        (separators-regexp "[,' ]"))
    (->> unclean
         (pg-pandoc--maybe-downcase)
         (replace-regexp-in-string separators-regexp "-")
         (replace-regexp-in-string bad-chars-regexp "")
         (pg-pandoc--maybe-remove-leading-hyphens))))

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

(defvar-local pg-pandoc-output-file-basename nil
  "Local variable defining the base output file name for
`pg-pandoc-current-buffer' and `pg-pandoc-org-subtree'. This
value will be preferred over other methods of determining the
base output file name." )

(defcustom pg-pandoc-default-output-basename "pandoc-output"
  "If no other sensible default for output filename can be
identified, use this.")

(defcustom pg-pandoc-org-output-prefer 'title
  "Specify which heuristic to prefer when converting an org-mode
document. Valid values are 'title (prefer the value of the TITLE
document keyword) and 'file (prefer basename of the buffer
file). If one does not exist, the other is used. If neither
exists, a fixed default is used. It is assumed that only one
TITLE keyword is defined.

If `pg-pandoc-output-file-basename' is set, this variable is ignored.")

;; Output file name generators.

;; Note that all these will eventually be passed through
;; pg-pandoc--sanitize-for-file-name to ensure the name consists
;; entirely of legal characters. These functions may do additional
;; work to convert characters in a context-sensitive way. Slashes,
;; for instance, may treated differently if they appear in labels or
;; file names, for example.

(defun pg-pandoc--ofn-org ()
  "Return an output file base name for Org Mode buffers.

This function assumes the whole buffer will be
converted. `pg-pandoc-org-subtree' uses its own logic for
determining the output file base name."

  (let* ((buf-fname (if (buffer-file-name)
                        (file-name-base (buffer-file-name))
                      nil))
         (doc-title (-some->> (org-ml-parse-this-buffer)
                      (pg-org-get-keyword-value "TITLE")
                      (replace-regexp-in-string "\s*:\s*" "_")))
         (choice (if (eq pg-pandoc-org-output-prefer 'title)
                     (or doc-title buf-fname)
                   (or buf-fname doc-title))))
    ;; Did both choices suck? Return the default.
    (or choice pg-pandoc-default-output-basename)))


(defun pg-pandoc--ofn-org-src ()
  "Return an output file base name for Org Src edit buffers."
  ;; If the block name is defined, use it. Otherwise, switch to the
  ;; parent buffer and run pg-pandoc--output-fname-base to determine
  ;; what its output file name would be. Then use that with a hyphen
  ;; appended. (Assumption: User will provide additional
  ;; qualifications.)
  ;;
  ;; Call pg-pandoc--output-fname-base instead of the org-buffer
  ;; specific ofn function in order to get additional default
  ;; processing, such as the value of pg-pandoc-output-file-basename for
  ;; the parent buffer.
  (let ((block-name (pg-org-src-info-get :name)))
    (if block-name
        (replace-regexp-in-string "/" "_" block-name)
      (let ((parent-name (save-excursion
                           (set-buffer (pg-org-src-parent-buffer))
                           (pg-pandoc--output-fname-base))))
        (concat parent-name "-")))))

(defun pg-pandoc--ofn-org-capture ()
  "Return an output file base name for Org Capture edit buffers"
  (or
   (->> (org-ml-parse-headline-at (point-min))
        (org-ml-get-property :raw-value)
        (replace-regexp-in-string "^\\[.*\\] " "") )
   (error "Can't read initial headline. Malformed capture buffer?")))



(defun pg-pandoc--generate-output-file-basename ()
  "Determine what should be used as the base filename for this
buffer.

The value of the buffer--local variable
`pg-pandoc-output-file-basename' is always used to determine the
initial base filename if it is defined. Otherwise:

- If the buffer is an Org Mode buffer, use the value of the
TITLE keyword or the buffer file name, depending on the value of
`pg-pandoc-org-output-prefer'.

- If the buffer is a named Org Src edit buffer, the block name is
used. Slashes in the name are converted to underscores to prevent
misinterpretation as directory separators. If the block is
unnamed, the output filename of the parent Org file is used, with
a hyphen appended; the user is expected to further qualify the
name.

- If the buffer is an Org Capture buffer, the text of the first
headline is used, minus any leading timestamp. If the headline
does not exist, this is an error.

- If the buffer is none of these but has a file name, the
  basename of the file name is used.

In all other cases the value of
`pg-pandoc-default-output-basename' is used.

After the initial base filename is chosen, it is sanitized by
`pg-pandoc--sanitize-for-file-name' before being returned."

  (->> (or pg-pandoc-output-file-basename
           (cond
            ((org-capture-get :original-buffer t)
             (pg-pandoc--ofn-org-capture))
            ((org-src-edit-buffer-p)
             (pg-pandoc--ofn-org-src))
            ((eq major-mode 'org-mode)
             (pg-pandoc--ofn-org))
            ((buffer-file-name)
             (->> (buffer-file-name)
                  (file-name-base)))
            (t
             pg-pandoc-default-output-basename)))
       (pg-pandoc--sanitize-for-file-name)))

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



;;
;; Public functions:
;;

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
         (fname-base (pg-pandoc--generate-output-file-basename))
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
