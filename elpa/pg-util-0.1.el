;;; pg-util.el --- Utility functions

;; Copyright (C) 2017 Phil Groce

;; Author: Phil Groce <pgroce@gmail.com>
;; Version: 0.1
;; Keywords: gui

(require 'cl)

(defmacro pg/spy (x)
  "Print the form and value of x to the message buffer, then
return x. x is only evaluated once."
  (let ((pg/x (make-symbol "x")))
    `(let ((,pg/x ,x)) (message (format "%s = %s" ',x  ,pg/x)) ,pg/x)))

(defun pg/zip (lists)
  "Return list of lists where each element of index n of the
output list is a list of all elements of index n of the input
lists. For instance: (pg/zip '(1 2 3) '(4 5 6) '(7 8 9)) -> '((1
4 7) (2 5 8) (3 6 9))"
  (apply #'mapcar* #'list lists))

(defun pg/list-add-unique (l1 l2)
  "Return a new list composed of the values in L1 and the values
  in L2 that don't already exist in L1. (IOW, L1 + L2, with no
  duplications from L2.)

  If there are duplicate values in L1, these will be preserved."
  (-concat l1 (--filter (not (memq it l1)) l2)))

(defun pg/list-add-unique-var (var values)
  "Update the list in VAR with the new values in VALUES using
  `pg/list-add-unique'. (Deprecated alias: `add-unique'.)"
  (set var (pg/list-add-unique (symbol-value var) values)))

(defalias 'add-unique 'pg/list-add-unique-var)

(defun pg/alist-update (a1 a2)
  "Return a new alist with the elements in A1, updated by A2. If
an element key exists in A1 and A2, it is updated in-place with
the value from A2. Elements in A2 with keys that are not in A1
are appended to the end of the new alist."
  (let ((a2-only (--filter (eq nil (assoc (car it) a1)) a2))
        (updated-a1 (--map (or (assoc (car it) a2) it) a1)))
    (-concat updated-a1 a2-only)))

(defun pg/alist-update-var (var values)
  "Update the alist in VAR with the new values in VALUES using
`pg/alist-update'. Shorthand for `(set
var (pg/alist-update (symbol-value-var) values)'."
  (set var (pg/alist-update (symbol-value var) values)))


(defun pg/alist-keys (in-alist)
  "Return a list of the keys in IN-ALIST, an associative list."
  (if (null in-alist)
      nil
    (cons (caar in-alist) (pg/alist-keys (cdr in-alist)))))


(defun pg/plist-keys (in-plist)
  "Return a list of the keys in IN-PLIST, a property list."
  (if (null in-plist)
      nil
    (cons (car in-plist) (pg/plist-keys (cddr in-plist)))))


;;; Note that hash-table-keys and hash-table-values live in subr-x

(defun pg/update-auto-mode-alist (ext new-mode &optional amalist)
  "Return a new copy of AMALIST (if nil, use `auto-mode-alist')
in which the mode function for EXT is replaced with NEW-MODE."

  (let* ((amalist (if amalist amalist auto-mode-alist))
         (filtered-amalist (--filter (not (equal ext (car it)))
                                        amalist)))
    (add-to-list 'filtered-amalist `(,ext . ,new-mode))))

(defun pg/minor-mode-active-p (minor-mode)
  "Return t if the minor mode is active in the current buffer,
otherwise nil."
  (condition-case nil
      (and (symbolp minor-mode) (symbol-value minor-mode))
    ('error nil)))

(defun pg/nuke-kill-ring ()
  "Try to annihilate all history of anything recently typed,
copied or pasted."
  (setq kill-ring nil)
  (setcdr yank-menu nil)
  (clear-this-command-keys)
  ;; Things get ugly with various histories, but try a little
  (setq minibuffer-history '()))

(provide 'pg-util)
;;; pg-util.el ends here
