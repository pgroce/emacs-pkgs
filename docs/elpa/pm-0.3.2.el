;;; pm.el --- Project Management

;; Copyright (C) 2021 Phil Groce

;; Author: Phil Groce <pgroce@gmail.com>
;; Version: 0.3.2
;; Package-Requires: ((emacs "26.1") (dash "2.19") (s "1.12") (org-ml "5.7") (ts "0.3") (projectile "20210825.649" (helm "20210826.553")))
;; Keywords: productivity

(require 'dash)
(require 's)
(require 'ts)
(require 'org-ml)
(require 'projectile)
(require 'helm)

(defcustom pg-pm-project-dir "~/active-projects"
  "Directory containing projects"
  :type 'directory
  :group 'pm)

(defun pg-pm-set-agenda-files ()
  "Set `org-agenda-files' according to the contents of
  `pg-pm-active-projects'. Called as a hook in
  `pg-pm-active-projects-refreshed-hook'."
  (setq org-agenda-files (pg-pm-active-projects))
  (message "pm: Agenda refreshed"))

(defcustom pg-pm-active-projects-refreshed-hook
  '(pg-pm-set-agenda-files)
  "Hook run when the active projects are refreshed."
  :group 'pm
  :type 'hook)


(defvar pg-pm--active-project-cache nil
  "List of active projects. Automatically generated if
  `nil'. Otherwise it must be manually refreshed using
  `pg-pm-refresh-active-projects' if new pm projects are
  created/removed.")







(defun pg-pm--find-active-projects ()
  "Find active project files on disk."
  ;; Visit the project file buffers and figure out which ones have an
  ;; active status. Don't keep any of the buffers around that weren't
  ;; around already.
  (--filter (let ((new? (not (find-buffer-visiting it))))
              (with-current-buffer (find-file-noselect it)
                (unwind-protect
                 (org-ml-match
                  '((:and keyword (:key "PROJECT_STATUS") (:value "active")))
                  (org-ml-parse-this-toplevel-section))
                 (when new?
                   (kill-buffer)))))
            (directory-files-recursively
             pg-pm-project-dir "^project.org$")))

(defun pg-pm--initialize-active-projects (&optional should-refresh? no-hooks?)
  "Initialize the list of active projects if it is
  uninitialized. If SHOULD-REFRESH? is non-nil, refresh
  the (non-empty) list.

Calling this function will run the hooks in
`pg-pm-active-projects-refreshed-hook' if the active projects are
refreshed; set NO-HOOKS? to a non-nil value to disable this
behavior."
  (when (or should-refresh?
            (eq nil pg-pm--active-project-cache))
    (setq pg-pm--active-project-cache (pg-pm--find-active-projects))
    (if  no-hooks?
        (message "pm: Not running hooks, no-hooks? is %s" no-hooks?)
      (run-hooks 'pg-pm-active-projects-refreshed-hook))))

;;;###autoload
(defun pg-pm-refresh-active-projects ()
  "Refresh the list of active projects', then run
`pg-pm-active-projects-refreshed-hook'. Run this command when
the active projects have changed on-disk, to get the list in
sync."
  (interactive)
  (pg-pm--initialize-active-projects t)
  (message "Active projects list refreshed"))

(defun pg-pm-active-projects ()
  "Return the list of active projects."
  (pg-pm--initialize-active-projects)
  pg-pm--active-project-cache)



(defun pg-pm--projectile-switch-project-action ()
  (let* ((org-files-source
          (helm-build-sync-source "Project Org Files"
            :candidates (->>  (directory-files ".")
                              (--filter (s-ends-with? ".org" it))
                              (--map (cons it it )))))
         (result (helm
                  :sources (list org-files-source
                                 helm-source-projectile-buffers-list
                                 helm-source-projectile-files-list)
                  :buffer "*helm PM project*"
                  :prompt (format "[%s] pattern: " (projectile-project-name)))))
    (cond
     ((stringp result) (find-file result))
     ((bufferp result) (switch-to-buffer result))
     (t result))))

;;;###autoload
(defun pg-pm-switch-to-active-project (&optional arg)
  "Switch to one of the acive projects"
  (interactive)
  (let ((proj (->> (pg-pm-active-projects)
                   (-map #'file-name-directory)
                   (completing-read "Switch to Active Project: ")))
        (projectile-switch-project-action
         #'pg-pm--projectile-switch-project-action))
    (projectile-switch-project-by-name proj arg)))

(defmacro pm--to-buffer (buffer-or-file-name &optional err-message)
  "If BUFFER-OR-FILE-NAME is a buffer, return it. If it's a
  string, try to open it as a file name. Otherwise, signal an
  error with ERR-MESSAGE, or a default message."
  (let ((err-message (if err-message
                         err-message
                       "Invalid parameter, must be buffer or file name.")))
    `(let ((b-or-fn ,buffer-or-file-name))
       (cond
        ((bufferp b-or-fn) b-or-fn)
        ((stringp b-or-fn) (find-file-noselect b-or-fn))
        (nil (error ,err-message))))))


(defun pm-project-meta (key project-file-or-buffer)
  "Assuming KEY is a keyword associated with the toplevel section
of the project file in PROJECT-FILE-OR-BUFFER, return the
value. IF the keyword is defined multiple times, get the first
value. If KEY is not defined, return nil."
  (let ((buff (pm--to-buffer
                project-file-or-buffer
                "Invalid parameter: must be project file name or buffer.")))
    (with-current-buffer buff
      (->> (org-ml-parse-this-toplevel-section)
           (org-ml-match `((:and keyword (:key ,key))))
           (--map (org-ml-get-property :value it))
           (first)))))

(defun pg-pm--accandidates (node)
  "Return headline nodes for all tasks under NODE with the keyword DONE.

As a practical matter, NODE can be a list of subtrees (i.e., the
return value of `org-ml-parse-subtrees')"
  (org-ml-match '(:any * (:and headline (:todo-keyword "DONE"))) node))

(defcustom pg-pm-project-file-logging-config
  '(:log-into-drawer "LOGBOOK" :clock-into-drawer t)
  "Logging format for drawers in project files.")


(defun pg-pm--headline-logbook-items (headline)
  "Use `org-ml-headline-get-logbook-items' to pull logbook items
off HEADLINE."
  (org-ml-headline-get-logbook-items
   pg-pm-project-file-logging-config
   headline))

(defcustom pg-pm-rx-logbook-resolved
  (rx "State"
      (+ whitespace)
      "\"" (group (+ (not "\""))) "\""
      (+ whitespace)
      "from"
      (+ whitespace)
      "\"" (group (+ (not "\""))) "\"")
  "Regex matching log entries of to-do state transitions, per the
  default state format string in
  `org-log-note-headings'. Capturing accomplishments will break
  if that entry in `org-log-note-headings' is changed. (As will
  large chunks of org-agenda.) In that case, it will be necessary
  to customize this regex to correspond.")

(defun pg-pm--parse-strans-log-entry (lb-item)
  "If LB-ITEM is a logbook entry that looks like it was generated
when a to-do item's status changed, parse it and return a list of
the state it was changed to (as a symbol), the state it was
changed from (as a symbol), the timestamp, and an org paragraph
element representing any additional notes provided by the
user. Otherwise, return nil."
  ;; Start by getting the paragraph portion of the logbook item
  (-when-let* [((s ts . the-rest)  (org-ml-item-get-paragraph lb-item))
               ;; parse out the to and from states
               ((_ to from) (->> (org-ml-to-trimmed-string s)
                                 (s-match pg-pm-rx-logbook-resolved)))
               ;; if notes exist, create as new paragraph
               (notes (if (org-ml-is-type 'line-break (first the-rest))
                          ;; trick to inline (cdr the-rest) as args
                          (let ((para-objs (-map (lambda (x) `(quote ,x)) (cdr the-rest))))
                            (eval `(org-ml-build-paragraph ,@para-objs)))
                        ;; no additional notes == empty paragraph
                        (org-ml-build-paragraph)))]
    (list (intern to) (intern from) ts notes)))


(defun pg-pm--strans-to-string (strans)
  "Render the data structure returned by
  `pg-pm--parse-strans-log-entry' as a string."
  (-let [(to from ts notes) strans]
    (format "#(%s %s \"%s\" \"%s\")"
            (symbol-name to)
            (symbol-name from)
            (org-ml-to-trimmed-string ts)
            (org-ml-to-trimmed-string notes))))

(defun pm-time-spec-from-string (time-spec)
  "Return a list of adjustments based on TIME-SPEC.

The format of TIME-SPEC is a series of adjustments of the form \"<num><unit>\",
where num is an integer (possibly negative) and unit is one of the following unit specifiers:

   Y : year
   M : month
   d : day
   h : hour
   m : minute
   s : second

For example, \"4y\" represents an adjustment of four years,
or ('year 4) as a `ts-adjust' adjustment. \"3d14h\"
represents ('day 3 'hour 14). Otherwise, all semantics of
`ts-ajust' are observed."
  (let ((s2 time-spec)
        (regex (rx bol
                   (group (* (or "+" "-"))
                          (+ digit))
                   (group (or "Y" "M" "d" "h" "m" "s"))))
        (unit-alist '(("Y" . year)
                      ("M" . month)
                      ("d" . day)
                      ("h" . hour)
                      ("m" . minute)
                      ("s" . second))))
    (cl-loop until (s-equals? "" s2)
             collect (-let [(all num unit) (s-match regex s2)]
                       (if (eq all nil)
                           (error "Invalid time spec '%s'" time-spec)
                         (progn
                           (setq s2 (substring-no-properties s2 (length all)))
                           (list (cdr (assoc unit unit-alist))
                                 (string-to-number num))))))))


(defun pm-ts-adjust-from-string (time-spec-string ts)
  "Like `ts-adjust', but instead of an series of adjustments,
adjust from a string representation derived from
`pg-time-spec-from-string'. TIME-SPEC-STRING contains the
adjustment string; it is applied to TS.

For the format of TIME-SPEC-STRING, see
`pg--time-spec-from-string'."
  (eval `(ts-adjust
          ,@(->> (pm-time-spec-from-string time-spec-string)
                 (-flatten)
                 (-map (lambda (x) `(quote ,x))))
          ,ts)))

(defun pg-pm--accomplishment? (headline strans-entries)
  "Returns a true value if the entries in STRANS-ENTRIES
  constitute an actual accomplishment, otherwise nil.

STRANS-ENTRIES should be a list of state transition logbook
entries, as processed by `pg-pm--parse-strans-log-entry'."
  ;; To be an accomplishment, there must be a logbook entry
  ;; corresponding to the current to-do state of the headline (so the
  ;; info in the first logbook entry and the headline to-do state must
  ;; match), and the to-do state of the headline must indicate that
  ;; the task is finished (which currently just means it's in state
  ;; DONE).
  ;;
  ;; If more than one to-do state indicated that a task was finished,
  ;; we'd also have to check that the state on the entry matched the
  ;; one on the headline, but with one finishing state, we get that
  ;; for free, so to speak.
  (and (equal "DONE" (org-ml-get-property :todo-keyword headline))
       (equal 'DONE (first (first strans-entries)))))


(defun pg-pm--build-accomplishment (headline)
  "Return an accomplishment record for HEADLINE. The
accomplishment record contains the headline, the transition log
entry corresponding to the finishing of the accomplishment, and
all the elements of the transition log entry, as returned by
`pg-pm--parse-strans-log-entry'.

If the headline is not, in fact, an accomplishment, this function
returns nil."
  (let ((logbook-entries (->> headline
                              (pg-pm--headline-logbook-items)
                              (-map #'pg-pm--parse-strans-log-entry))))
    (when (pg-pm--accomplishment? headline logbook-entries)
      (list headline (or (first logbook-entries)
                         (org-ml-get-property :title headline))))))


(defun pg-pm--accomplishment-headline (accomplishment)
  "Get the headline associated with ACCOMPLISHMENT."
  (-let [(headline _) accomplishment]
    headline))

(defun pg-pm--accomplishment-strans (accomplishment)
  "Get the state transition entry associated with ACCOMPLISHMENT."
  (-let [(_ strans) accomplishment]
    strans))

(defun pg-pm--accomplishment-to-string (accomplishment)
  "Render the data structure returned by
  `pg-pm--build-accomplishment' as a string."
  (-let [(headline strans) accomplishment]
    (format "#(\"%s\" %s)"
            (org-ml-to-trimmed-string headline)
            (pg-pm--strans-to-string strans))))

(defun pg-pm--accomplishments-build-plain-list (acc-items)
  "Build a plain-list with ACC-ITEMS as a list, not inlined."
  (if acc-items
      (eval `(org-ml-build-plain-list ,@(-map (lambda (x) `(quote ,x)) acc-items)))
    (org-ml-build-plain-list)))


(defun pg-pm--accomplishments-build-headline (project-name accomplishments)
  "Turn a list of accomplishments to an org-element headline."
  (->> accomplishments
       (-map #'pg-pm--accomplishment-build-item)
       (pg-pm--accomplishments-build-plain-list)
       (org-ml-build-section)
       (org-ml-build-headline
        :title (org-ml-build-secondary-string! project-name))))

(defun pg-pm--format-time(org-ts)
  (->> (ts-parse-org-element org-ts)
       (ts-format "%d %b")
       (format "/%s/")))

(defun pg-pm--accomplishment-build-item (accomplishment)
  "Convert ACCOMPLISHMENT, an accomplishment record, to an
  org item representation. If ACCOMPLISHMENT is nil, return
  nil."
  (-let* (((headline (_ _ ts notes)) accomplishment)
          (ts-formatted (pg-pm--format-time ts))
          (notes-formatted (org-ml-to-trimmed-string notes))
          (para-string (format "%s: %s" ts-formatted notes-formatted)))
    (org-ml-build-item! :paragraph para-string)))

(defun pg-pm--recent-accomplishments? (beg end accomplishment)
  "Return ACCOMPLISHMENT if its timestamp "
  (-let* (((_ (_ _ ts _)) accomplishment)
          (ts (ts-parse-org-element ts)))
    ;; ts-in is beg <= ts <= end, which means a time could be in
    ;; ranges a->b and in b->c. Defining it this way makes binning
    ;; easier. Which I won't be doing, so I don't know why I care, but
    ;; that's the story of my life.
    (when (and (ts< beg ts)
               (ts>= end ts))
      accomplishment)))

(defun pg-pm--beginning-time (end time-offset)
  "Return the beginning of a time range ending with END and
  defined relative to END by TIME-OFFSET. Signal an error if
  TIME-OFFSET is net positive (i.e., if the beginning would be
  after the end)."
  (let ((beg (pm-ts-adjust-from-string time-offset end)))
    (if (ts< end beg)
        (error "Beginning time is in the future (use negative offsets)")
      beg)))


(defun pg-pm--headlines-from-project-file (begin end project-file-name)
  "Return an alist entry of accomplishments, keyed by project name."
  (with-current-buffer (find-file-noselect project-file-name)
    (let ((project-name (pm-project-meta "TITLE" project-file-name)))
      (->> (org-ml-parse-subtrees 'all)
           (pg-pm--accandidates)
           (-keep #'pg-pm--build-accomplishment)
           (-keep (-partial #'pg-pm--recent-accomplishments?
                            begin end))
           (pg-pm--accomplishments-build-headline project-name)))))


;;  headline -> section -> plain-list -> [item -> paragraph]



(defun pg-pm-accomplishment-report (&optional time-offset)
  "Compile an accomplishment report from the tasks that have
been closed in a time period. Accomplishments are extracted from
the files returned by `pg-pm-active-projects'.

If TIME-OFFSET is nil, prompt the user for a time specification,
indicating how old an accomplishment can be before it is included
in the report. The syntax for this specification is given in
`pg-time-spec-from-string'."
  (interactive)
  (let* ((time-offset (or time-offset
                          (read-string "Find since: " "-7d")))
         (end (ts-now))
         (begin (pg-pm--beginning-time end time-offset))
         (toplevel (org-ml-build-section
                    (org-ml-build-keyword "TITLE" "Accomplishments Report")))
         (headlines
          (--map (pg-pm--headlines-from-project-file begin end it)
                 (pg-pm-active-projects)))
         (buff (get-buffer-create "*Accomplishments*")))
    (with-current-buffer buff
      (erase-buffer)
      (cd pg-pm-project-dir)
      (org-mode)
      (org-indent-mode)
      (insert (org-ml-to-string toplevel))
      (--map
       (insert (org-ml-to-string it))
       headlines))
    (switch-to-buffer buff)))

(provide 'pm)
;;; pm.el ends here
