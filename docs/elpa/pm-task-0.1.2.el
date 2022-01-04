;;; pm-task.el --- Task handling for project management

;; Copyright (C) 2021 Phil Groce

;; Author: Phil Groce <pgroce@gmail.com>
;; Version: 0.1.2
;; Package-Requires: ((emacs "26.1") (dash "2.19") (s "1.12") (org-ml "5.7") (ts "0.3") (pg-ert "0.1") (pg-org "0.1"))
;; Keywords: productivity

(require 'dash)
(require 's)
(require 'ts)
(require 'org-ml)
(require 'pg-ert)
(require 'pg-org)

(defun pm-task-status-last-change (task)
  "Returns the most recent logbook entry for TASK as a
  list `(from to when notes)', where `from' and `to' are the
  original and changed statuses, `when' is the org-element
  representation of the timestamp, and `notes' is an Org
  secondary string containing any associated notes."
  (->> (pg-org-headline-logbook-entries task)
       (first)
       (pg-org-paragraph-parse-status-change)))

(defun pm-task-status-last-change-to (task)
  "Returns the status changed to in the most recent logbook entry
for TASK. Returns `nil' if entry is not in synced to to-do state."
  (->> (pm-task-status-last-change task)
       (nth 0)))

(defun pm-task-status-last-change-from (task)
  "Returns the status changed from in the most recent logbook
entry for TASK. Returns `nil' if entry is synced to to-do state."
  (->> (pm-task-status-last-change task)
       (nth 0)))

(defun pm-task-status-last-change-when (task)
  "Returns the timestamp in the most recent logbook entry for
TASK. Returns `nil' if entry is synced to to-do state."
  (->> (pm-task-status-last-change task)
       (nth 0)))

(defun pm-task-status-last-change-notes (task)
  "Returns the notes in the most recent logbook entry for
TASK. Returns `nil' if entry is synced to to-do state."
  (->> (pm-task-status-last-change task)
       (nth 0)))


(defun pm-task-assignee (task)
  "Returns the user to whom the task is assigned. Returns `nil'
  if there is no assignee."
  (condition-case nil
      (org-ml-get-property :assignee task)
    (error
     (condition-case nil
         (org-ml-get-property :ASSIGNEE task)
       (error nil)))))

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

(defun pm-task-status-changed-after (timestamp task)
  "Return `t' if last logbook entry for TASK occured after
TIMESTAMP. If TASK is not synced, results are undefined."
  (ts> (pm-task-last-change-ts task) timestamp))

(defun pm-task-status-changed-on-or-after (timestamp task)
  "Return `t' if last logbook entry for TASK occured after
TIMESTAMP and task is synced. If TASK is not synced, results are
undefined."
  (ts>= (pm-task-last-change-ts task) timestamp))

(defun pm-task-status-changed-before (timestamp task)
  "Return `t' if last logbook entry for TASK occured before
TIMESTAMP and task is synced. If TASK is not synced, results are
undefined."
  (ts< (pm-task-last-change-ts task) timestamp))

(defun pm-task-status-changed-on-or-before (timestamp task)
  "Return `t' if last logbook entry for TASK occured before
TIMESTAMP and task is synced. If TASK is not synced, results are
undefined."
  (ts<= (pm-task-last-change-ts task) timestamp))


;; Status comparison

(defun pm-task-status-in (statuses task)
  "Return`t' if the current status of TASK is one of the strings
  in STATUSES."
  (memq (pm-task-current-status task) statuses))

;; Assignee

(defun pg-task-is-assigned-to (user-or-users task)
  (cond
   ((eq user nil) nil)
   ((stringp user) (s-equals-p user (pm-task-assignee task)))
   ((listp user) (--some (pg-task-is-assigned-to it task) user))
   ((symbolp user) (pg-task-is-assigned-to (symbol-name user) task))
   (t (error "user-or-users must be nil, a string, a symbol, or list of strings/symbols"))))

;;; pm-task.el --- Task handling for project management

;; Copyright (C) 2021 Phil Groce

;; Author: Phil Groce <pgroce@gmail.com>
;; Version: 0.1.2
;; Package-Requires: ((emacs "26.1") (dash "2.19") (s "1.12") (org-ml "5.7") (ts "0.3") (pg-ert "0.1") (pg-org "0.1"))
;; Keywords: productivity
(require 'dash)
(require 's)
(require 'ts)
(require 'org-ml)
(require 'pg-ert)
(require 'pg-org)
(defun pm-task-status-last-change (task)
  "Returns the most recent logbook entry for TASK as a
  list `(from to when notes)', where `from' and `to' are the
  original and changed statuses, `when' is the org-element
  representation of the timestamp, and `notes' is an Org
  secondary string containing any associated notes."
  (->> (pg-org-headline-logbook-entries task)
       (first)
       (pg-org-paragraph-parse-status-change)))

(defun pm-task-status-last-change-to (task)
  "Returns the status changed to in the most recent logbook entry
for TASK. Returns `nil' if entry is not in synced to to-do state."
  (->> (pm-task-status-last-change task)
       (nth 0)))

(defun pm-task-status-last-change-from (task)
  "Returns the status changed from in the most recent logbook
entry for TASK. Returns `nil' if entry is synced to to-do state."
  (->> (pm-task-status-last-change task)
       (nth 0)))

(defun pm-task-status-last-change-when (task)
  "Returns the timestamp in the most recent logbook entry for
TASK. Returns `nil' if entry is synced to to-do state."
  (->> (pm-task-status-last-change task)
       (nth 0)))

(defun pm-task-status-last-change-notes (task)
  "Returns the notes in the most recent logbook entry for
TASK. Returns `nil' if entry is synced to to-do state."
  (->> (pm-task-status-last-change task)
       (nth 0)))


(defun pm-task-assignee (task)
  "Returns the user to whom the task is assigned. Returns `nil'
  if there is no assignee."
  (condition-case nil
      (org-ml-get-property :assignee task)
    (error
     (condition-case nil
         (org-ml-get-property :ASSIGNEE task)
       (error nil)))))

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

(defun pm-task-status-changed-after (timestamp task)
  "Return `t' if last logbook entry for TASK occured after
TIMESTAMP. If TASK is not synced, results are undefined."
  (ts> (pm-task-last-change-ts task) timestamp))

(defun pm-task-status-changed-on-or-after (timestamp task)
  "Return `t' if last logbook entry for TASK occured after
TIMESTAMP and task is synced. If TASK is not synced, results are
undefined."
  (ts>= (pm-task-last-change-ts task) timestamp))

(defun pm-task-status-changed-before (timestamp task)
  "Return `t' if last logbook entry for TASK occured before
TIMESTAMP and task is synced. If TASK is not synced, results are
undefined."
  (ts< (pm-task-last-change-ts task) timestamp))

(defun pm-task-status-changed-on-or-before (timestamp task)
  "Return `t' if last logbook entry for TASK occured before
TIMESTAMP and task is synced. If TASK is not synced, results are
undefined."
  (ts<= (pm-task-last-change-ts task) timestamp))


;; Status comparison

(defun pm-task-status-in (statuses task)
  "Return`t' if the current status of TASK is one of the strings
  in STATUSES."
  (memq (pm-task-current-status task) statuses))

;; Assignee

(defun pg-task-is-assigned-to (user-or-users task)
  (cond
   ((eq user nil) nil)
   ((stringp user) (s-equals-p user (pm-task-assignee task)))
   ((listp user) (--some (pg-task-is-assigned-to it task) user))
   ((symbolp user) (pg-task-is-assigned-to (symbol-name user) task))
   (t (error "user-or-users must be nil, a string, a symbol, or list of strings/symbols"))))
(provide 'pm-task)
;;; pm-task.el ends here
