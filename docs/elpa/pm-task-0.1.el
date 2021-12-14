;;; pm-task.el --- Task handling for project management

;; Copyright (C) 2021 Phil Groce

;; Author: Phil Groce <pgroce@gmail.com>
;; Version: 0.1
;; Package-Requires: ((emacs "26.1") (dash "2.19") (s "1.12") (org-ml "5.7") (ts "0.3") (pg-ert "0.1") (pg-org "0.1"))
;; Keywords: productivity

(require 'dash)
(require 's)
(require 'ts)
(require 'org-ml)
(require 'pg-ert)
(require 'pg-org)

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
  to customize this regex to correspond."
  :type 'regexp
  :group 'pm)

(provide 'pm-task)
;;; pm-task.el ends here
