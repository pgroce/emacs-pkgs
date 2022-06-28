;;; pm-task.el --- Task handling for project management

;; Copyright (C) 2021 Phil Groce

;; Author: Phil Groce <pgroce@gmail.com>
;; Version: 0.1.5

;; Package-Requires: ((emacs "26.1") (dash "2.19") (s "1.12") (org-ml "5.7") (ts "0.3") (pg-ert "0.1") (pg-org "0.4"))
;; Keywords: productivity

(require 'dash)
(require 's)
(require 'ts)
(require 'org-ml)
(require 'pg-ert)
(require 'pg-org)

(defun pm-task-build (the-headline keyword description assignee user)
  (let* ((now-ts  (ts-now))
         (now-str (ts-format "[%Y-%m-%d %a %H:%M]" now-ts))
         (now-org `(timestamp 'inactive
                              ,(ts-year now-ts)
                              ,(ts-month now-ts)
                              ,(ts-day now-ts)
                              ,(ts-year now-ts)
                              ,(ts-month now-ts)
                              ,(ts-day now-ts)
                              :hour-start ,(ts-hour now-ts)
                              :minute-start ,(ts-minute now-ts)
                              :hour-end ,(ts-hour now-ts)
                              :minute-end ,(ts-minute now-ts)))
         (user (or user (user-login-name)))
         ;; no default for assignee; if it's nil, don't include it
         (the-headline (or the-headline ""))
         (keyword (or keyword "TODO"))
         (description (or description ""))
         (assignee (or assignee "")))
    `(headline
      :title (secondary-string! ,the-headline)
      :todo-keyword ,keyword
      (section
       (property-drawer
        ;; Interestingly, org-element stores node properties as strings,
        ;; including org-mode timestamps. But in a buffer, they're
        ;; handled as regular timestamps. ¯\_(ツ)_/¯
        (node-property "CREATED" ,now-str)
        (node-property "CREATOR" ,user)
        (node-property "ASSIGNEE" ,assignee)
        (node-property "CUSTOM_ID" ,(format "pm-task-%s" (org-id-uuid))))
       (drawer
        "LOGBOOK"
        :post-blank 1
        (plain-list
         (item (paragraph
                ,(format "Task created by \"%s\" on %s" user now-str)))))
       (paragraph! ,description)))))

(defun pm-task-get-logbook (task)
  "If TASK has a logbook, returns a copy of it as an org-element
expression. If not, returns a new, empty logbook."
  (let ((logbook-name (org-log-into-drawer)))
    (-if-let (logbook (org-ml-match
                       `(section (:and drawer (:drawer-name ,logbook-name)))
                       task))
        (nth 0 logbook)
      (org-ml-build-drawer logbook-name))))

(defun pm-task-set-logbook (new-logbook task)
  "Make NEW-LOGBOOK the logbook in TASK. If a logbook already
  exists in TASK, it is replaced with NEW-LOGBOOK."
  (let ((matcher
         '(:first section
                  (:and drawer
                        (:drawer-name "LOGBOOK")))))
    (if (org-ml-match matcher task)
        ;; Replace old logbook with new one
        (org-ml-match-replace matcher new-logbook task)
      ;; Insert new logbook after property drawer, if it exists. If
      ;; not, insert it as the first child.
      (let* ((section (org-ml-headline-get-section task))
             (sct-children (org-ml-get-children section))
             (pdrawer-idx (--find-index
                           (org-ml-is-type 'property-drawer)
                           sct-children))
             (new-children
              (if (numberp pdrawer-idx)
                  (-insert-at (+ 1 pdrawer-idx) new-logbook sct-children)
                (-insert-at 0 new-logbook sct-children))))
        (org-ml-headline-set-section new-children)))))

(defun pm-task-set-logbook-2 (task new-logbook)
  "Equivalent to `pm-task-set-logbook', but with arguments
  reversed for easier function composition."
  (pm-task-set-logbook new-logbook task))

(defun pm-task-log (header msg task)
  "Create a new task based on TASK, with a new log entry in the
logbook. The log entry will contain a header as specified by
HEADER, a space, a timestamp, a line break, and the contents of
MSG, a string."
  (let* ((now (->> (ts-now)
                   (pg-org-build-timestamp-from-ts nil)))
         (header (or header (format
                             "Log entry by \"%s\""
                             user-login-name)))
         (break (org-ml-build-line-break))
         (new-entry (if msg
                        (org-ml-build-paragraph
                         header " " now  " " break msg)
                      (org-ml-build-paragraph
                       header " " now))))
    (--> (pm-task-get-logbook task)
         (pg-org-logbook-prepend-paragraph new-entry it)
         (pm-task-set-logbook it task))))

;; This method of interpolation borrowed from org-ml--log-replace-*
(defun pm-task-annotate (msg task)
  "Annotate TASK with MSG. User and time are also logged in this
message. User is taken from `user-login-name'; if this is not
set, the empty string will be used."
  (pm-task-log
   (format "Annotation by \"%s\" on" (or user-login-name "")) msg task))

(defun pm-task-reassign (new-assignee task)
  (let* ((old-assignee (or (pm-task-assignee task) ""))
         (header
          (format
           "User \"%s\" changed assignee from \"%s\" to \"%s\""
           user-login-name old-assignee new-assignee)))
    (->> task
         (pg-org-headline-set-node-property
          nil 'replace
          "ASSIGNEE" (list new-assignee))
         (pm-task-log header nil))))

(defun pm-task--logbook-item-get-parts (item)
  "Return the different parts of ITEM.

Returns a three-element list of the message header (as a
paragraph object), the timestamp (as a timestamp object), and the
message (as a paragraph object)"
  (cl-letf (((symbol-function 'is-long-inactive-timestamp)
             (lambda (node)
               (when (and (org-ml-is-type 'timestamp node)
                          (org-ml--property-is-eq :type 'inactive node)
                          (-some->> (org-ml--timestamp-get-start-time node)
                            (org-ml-time-is-long)))
                 (org-ml--timestamp-get-start-unixtime node))))

            ((symbol-function 'is-line-break)
             (lambda (node)
               (or (org-ml-is-type 'line-break node)
                   (and (org-ml-is-type 'plain-text node)
                        (equal "\n" node)))))

            ((symbol-function 'get-paragraph-children)
             (lambda (item)
               (-when-let (first-child (car (org-ml-get-children item)))
                 (when (org-ml-is-type 'paragraph first-child)
                   (org-ml-get-children first-child))))))

    ;; Split the children on the line break, if it exists
    (-let* (((left right) (->> item
                               (get-paragraph-children)
                               (-split-when #'is-line-break)
                               ))
            (timestamp (when (is-long-inactive-timestamp (-last-item left))
                         (-last-item left)))
            (left-parts (if timestamp
                            (-slice left 0 -1)
                          left))
            (left-para (apply #'org-ml-build-paragraph left-parts))
            (right-para (apply #'org-ml-build-paragraph right)))
      (list left-para timestamp right-para))))

(defun pm-task-logbook-item-get-header (item)
  "Return the header associated with the logbook item ITEM."
  (nth 0 (pm-task--logbook-item-get-parts item)))

(defun pm-task-logbook-item-get-timestamp (item)
  "Return the timestamp associated with the logbook item ITEM."
  (nth 1 (pm-task--logbook-item-get-parts item)))

(defun pm-task-logbook-item-get-message (item)
  "Return the message associated with the logbook item ITEM."
  (nth 2 (pm-task--logbook-item-get-parts item)))

(defun pm-task-status-changes (task)
  "Returns all status change logbook entries for TASK as a list
    of records `(from to when notes)', where `from' and `to' are
    the original and changed statuses, `when' is the org-element
    representation of the timestamp, and `notes' is an Org
    secondary string containing any associated notes."
  (->> (pg-org-headline-logbook-entries task)
       (-keep #'pg-org-paragraph-parse-status-change)))

(defun pm-task-status-last-change (task)
  "Returns the most recent status change logbook entry for
    TASK. For the format of this record see
    `pm-task-status-changes'"
  (nth 0 (pm-task-status-changes task)))

(defun pm-task-status-last-change-from (task)
  "Returns the status changed from in the most recent logbook
  entry for TASK."
  (nth 0 (pm-task-status-last-change task)))

(defun pm-task-status-last-change-to (task)
  "Returns the status changed to in the most recent logbook entry
  for TASK."
  (nth 1 (pm-task-status-last-change task)))

(defun pm-task-status-last-change-timestamp (task)
  "Returns the timestamp in the most recent logbook entry for
  TASK, as a ts.el timestamp object."
  (->> (nth 2 (pm-task-status-last-change task))
       (ts-parse-org-element)))

(defun pm-task-status-last-change-org-timestamp (task)
  "Returns the timestamp in the most recent logbook entry for
  TASK, as an org-element."
  (nth 2 (pm-task-status-last-change task)))

(defun pm-task-status-last-change-notes (task)
  "Returns the notes in the most recent logbook entry for
  TASK."
  (nth 3 (pm-task-status-last-change task)))

  ;; I'm making a Big Assumption that entries in the logbook will
  ;; already be sorted by time.

(defun pm-task-current-status (task)
  "Returns the current status of TASK. The current status is the
    todo keyword of the headline; if the headline has no todo
    keyword, it is `nil'.

  This may not be synchronized with the most recent logbook entry;
  if the user wants this, they must call ()`pm-task-is-synced'
  first."
  (org-ml-get-property :todo-keyword task))

(defun pm-task-assignee (task)
  "Returns the user to whom the task is assigned. Returns `nil'
    if there is no assignee."
  (or (org-ml-headline-get-node-property "assignee" task)
      (org-ml-headline-get-node-property "ASSIGNEE" task)))

(defun pm-task-created-on (task)
  "Returns the time of this tasks creation, as a ts
    structure. Returns `nil' if TASK has no \"CREATED\" or
    \"created\" property drawer, or if the contents of that
    property are not a valid org-mode timestamp."
  (when-let* ((created (or (org-ml-headline-get-node-property "created" task)
                           (org-ml-headline-get-node-property "CREATED" task)))
              (created-ts (ts-parse-org created)))
    created-ts))

(defun pm-task-creator (task)
  "Returns the creator of this task, or `nil' if none is specified."
  (or (org-ml-headline-get-node-property "CREATOR" task)
      (org-ml-headline-get-node-property "creator" task)))

(defun pm-task-last-change (task)
  (cl-letf* (((symbol-function '-ts-val)
              (lambda (item)
                (-some->>
                    (org-ml-logbook-item-get-timestamp item)
                  (ts-parse-org-element))))

             ((symbol-function 'cmp)
              (lambda (l r)
                (let ((ts-l (-ts-val l))
                      (ts-r (-ts-val r)))
                  (and (and ts-l ts-r)
                       (ts< ts-l ts-r))))))
    (->> (pm-task-get-logbook task)
         (pg-org-logbook-get-items)
         (-sort 'cmp)
         (nth 0))))

;; Everything has to deal with unsynced tasks. It's the caller's
;; responsibility to check if the task is synced before using.


(defun pm-task-is-synced (task)
  "Return `t' if TASK has a to-do item that matches the current
  state from the last entry in the logbook. If not, the logbook
  can't be used to determine the time of the most recent status
  change or the previous status."
  (let ((todo (org-ml-get-property :todo-keyword task)))
    (and todo
         (equal todo (pm-task-status-last-change-to task)))))

;; Time comparison

(defun pm-task-status-changed-on (timestamp task)
  "Return `t' if last logbook entry for TASK is equal to
TIMESTAMP, a ts object. If TASK is not synced, results are
undefined."
  (ts=  timestamp (pm-task-status-last-change-timestamp task)))

(defun pm-task-status-changed-after (timestamp task)
  "Return `t' if last logbook entry for TASK occured after
TIMESTAMP, a ts object. If TASK is not synced, results are
undefined."
  (ts< timestamp (pm-task-status-last-change-timestamp task)))

(defun pm-task-status-changed-on-or-after (timestamp task)
  "Return `t' if last logbook entry for TASK occured after
TIMESTAMP (a ts object) and task is synced. If TASK is not
synced, results are undefined."
  (ts<=  timestamp (pm-task-status-last-change-timestamp task)))

(defun pm-task-status-changed-before (timestamp task)
  "Return `t' if last logbook entry for TASK occured before
TIMESTAMP (a ts object) and task is synced. If TASK is not
synced, results are undefined."
  (ts> timestamp (pm-task-status-last-change-timestamp task)))

(defun pm-task-status-changed-on-or-before (timestamp task)
  "Return `t' if last logbook entry for TASK occured before
TIMESTAMP and task is synced. If TASK is not synced, results are
undefined."
  (ts>= timestamp (pm-task-status-last-change-timestamp task)))

;; Status comparison

(defun pm-task-status-in (status-or-statuses task)
  "Return `t' if the current status of TASK is one of the strings
  in STATUS-OR-STATUSES, which can be a single string or list of
  strings. if STATUS-OR-STATUSES is `nil' (or a list where one of
  its elements is nil), this function will return nil."
  (cond
   ((eq status-or-statuses nil)
    (eq nil (pm-task-current-status task)))
   ((stringp status-or-statuses)
    (s-equals-p status-or-statuses (pm-task-current-status task)))
   ((listp status-or-statuses)
    (--some (pm-task-status-in it task) status-or-statuses))
   (t (error "status-or-statuses must a string or list of strings"))))

;; Assignee

(defun pm-task-is-assigned-to (user-or-users task)
  "Return `t' if the assignee of TASK is in USER-OR-USERS, which
can be a single string or a list of strings. If USER-OR-USERS is
`nil' (or a list where one of its elements is nil), this function
will return `t' if TASK is unassigned."
  (cond
   ((eq user-or-users nil)
    (eq nil (pm-task-assignee task)))
   ((stringp user-or-users)
    (s-equals-p user-or-users (pm-task-assignee task)))
   ((listp user-or-users)
    (--some (pm-task-is-assigned-to it task) user-or-users))
   (t (error "user-or-users must a string or list of strings"))))

(defun pm-ui--annotate-finish-function ()
  (if (not org-note-abort)
      (let ((txt (buffer-substring-no-properties (point-min) (point-max)))
            lines)
        ;; Strip comments and format a la org-mode
        ;; (especially org-store-log-note)
        (while (string-match "\\`# .*\n[ \t\n]*" txt)
          (setq txt (replace-match "" t t txt)))
        (when (string-match "\\s-+\\'" txt)
          (setq txt (replace-match "" t t txt)))
        (message txt))
    (message "aborted"))
  (kill-buffer))

(defun pm-ui-annotate ()                ;(task)
  "Create annotation for TASK, editing content of message in a temporary buffer"
  (interactive)
  (org-switch-to-buffer-other-window "*PM Task Annotate*")
  (erase-buffer)
  (let ((org-inhibit-startup t)) (org-mode))
  (insert "# Insert annotation. Finish with C-c C-c, cancel with C-c C-k\n\n")
  (setq-local org-finish-function #'pm-ui--annotate-finish-function))

(provide 'pm-task)
;;; pm-task.el ends here
