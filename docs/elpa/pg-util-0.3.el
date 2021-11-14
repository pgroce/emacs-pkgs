;;; pg-util.el --- Utility functions

;; Copyright (C) 2017-2020 Phil Groce

;; Author: Phil Groce <pgroce@gmail.com>
;; Version: 0.3
;; Package-Requires: ((dash "2.13.0"))
;; Keywords: utility

(require 'cl)
(require 'dash)

;;;###autoload
(defmacro pg-util-spy (x)
  "Print the form and value of x to the message buffer, then
return x. x is only evaluated once."
  (let ((pg-util-x (make-symbol "x")))
    `(let ((,pg-util-x ,x)) (message (format "%s = %s" ',x  ,pg-util-x)) ,pg-util-x)))

(defalias '/spy 'pg-util-spy)

(defcustom pg-error-on-deprecation nil
  "Set to `t' to change deprecation warnings to (traceable) errors.")

;;;###autoload
(defun pg-util-zip (lists)
  "Return list of lists where each element of index n of the
output list is a list of all elements of index n of the input
lists. For instance: (pg-util-zip '(1 2 3) '(4 5 6) '(7 8 9)) -> '((1
4 7) (2 5 8) (3 6 9))"
  (if pg-error-on-deprecation
      (error "pg-util-zip is deprecated. Use dash.el's -zip-lists instead.")
    (warn "pg-util-zip is deprecated. Use dash.el's -zip-lists instead."))
  (apply #'mapcar* #'list lists))


(defalias '/zip 'pg-util-zip)

;;;###autoload
(defun pg-util-list-add-unique (l1 l2)
  "Return a new list composed of the values in L1 and the values
  in L2 that don't already exist in L1. (IOW, L1 + L2, with no
  duplications from L2.)

  If there are duplicate values in L1, these will be preserved."
  (-concat l1 (--filter (not (memq it l1)) l2)))

(defalias '/list-add-unique 'pg-util-list-add-unique)

;;;###autoload
(defun pg-util-list-add-unique-var (var values)
  "Update the list in VAR with the new values in VALUES using
  `pg-util-list-add-unique'."
  (set var (pg-util-list-add-unique (symbol-value var) values)))

(defalias '/list-add-unique-var 'pg-util-list-add-unique-var)

;;;###autoload
(defun pg-util-list-update-1 (l elt matcher &optional no-match-action)
  "Update matching item in L with ELT.

This function returns a copy of L with the first matching item in
L replaced with ELT.  MATCHER takes an element in L and returns a
true value if they match and nil otherwise.  It is up to MATCHER
to define how items match.

NO-MATCH-ACTION determines what happens if no item in L matches
ELT.  If NO-MATCH-ACTION is the symbol `prepend', ELT will be
prepended onto L. If it is the symbol `append', ELT will be
appended to L. Otherwise (the default), L will not be changed."
  (let ((match-idx (-find-index matcher l)))
    (if (eq nil match-idx)
        (if (eq no-match-action 'prepend)
            (cons elt l)
          (if (eq no-match-action 'append)
              (append l (list elt))
            l))
      (-replace-at match-idx elt l))))

(defalias '/list-update-1 'pg-util-list-update-1)

;;;###autoload
(defun pg-util-list-update (l1 l2 matcher &optional no-match-action)
  "Update L1 with matching elements in L2, according to MATCHER.

This function returns a new list, with each element in L1 that
matches an element in L2 replaced with the matching element,
using `pg-util-list-update-1'.

MATCHER is a function that takes two arguments representing items
in L1 and L2, respectively, and returns a true value if they
\"match\", otherwise nil.

The semantics of NO-MATCH-ACTION are equivalent to those in
`pg-util-list-update-1'."
  (let ((rc l1))
    (cl-dolist (elt2 l2 rc)
      (let ((matcher-all matcher)
            (matcher-1 (lambda (elt1) (funcall matcher-all elt1 elt2))))
        (setq rc (pg-util-list-update-1
                  rc
                  elt2
                  matcher-1
                  no-match-action))))))

(defalias '/list-update 'pg-util-list-update)

;;;###autoload
(defun pg-util-alist-update (a1 a2)
  "Return a new alist with the elements in A1, updated by A2. If
an element key exists in A1 and A2, it is updated in-place with
the value from A2. Elements in A2 with keys that are not in A1
are appended to the end of the new alist."
  (let ((a2-only (--filter (eq nil (assoc (car it) a1)) a2))
        (updated-a1 (--map (or (assoc (car it) a2) it) a1)))
    (-concat updated-a1 a2-only)))

(defalias '/alist-update 'pg-util-alist-update)

;;;###autoload
(defun pg-util-alist-update-var (var values)
  "Update the alist in VAR with the new values in VALUES using
`pg-util-alist-update'. Shorthand for `(set
var (pg-util-alist-update (symbol-value-var) values)'."
  (set var (pg-util-alist-update (symbol-value var) values)))

(defalias '/alist-update-var 'pg-util-alist-update-var)


;;;###autoload
(defun pg-util-alist-keys (in-alist)
  "Return a list of the keys in IN-ALIST, an associative list."
  (if (null in-alist)
      nil
    (cons (caar in-alist) (pg-util-alist-keys (cdr in-alist)))))

(defalias '/alist-keys 'pg-util-alist-keys)


;;;###autoload
(defun pg-util-plist-keys (in-plist)
  "Return a list of the keys in IN-PLIST, a property list."
  (if (null in-plist)
      nil
    (cons (car in-plist) (pg-util-plist-keys (cddr in-plist)))))

(defalias '/plist-keys 'pg-util-plist-keys)


;;; Note that hash-table-keys and hash-table-values live in subr-x

;;;###autoload
(defun pg-util-update-auto-mode-alist (ext new-mode &optional amalist)
  "Return a new copy of AMALIST (if nil, use `auto-mode-alist')
in which the mode function for EXT is replaced with NEW-MODE."

  (let* ((amalist (if amalist amalist auto-mode-alist))
         (filtered-amalist (--filter (not (equal ext (car it)))
                                        amalist)))
    (add-to-list 'filtered-amalist `(,ext . ,new-mode))))

(defalias '/update-auto-mode-alist 'pg-util-update-auto-mode-alist)

(defun pg-util-prioritize (prioritized-items l)
  "Return L, with any items in PRIORITIZED-ITEMS moved to the top of the list, in the order they are specified. The order of any other items in the list is unchanged. If items in PRIORITIZED-ITEMS are not in L, they will not exist in the output.

Examples:

(pg-util-prioritize '(e d) '(a b c d e)) => '(e d a b c)
(pg-util-prioritize nil '(a b c d e)) => '(a b c d e)
(pg-util-prioritize '(e d) '(a b c)) => '(a b c)"
  (assert (listp prioritized-items))
  (assert (listp l))
  (setq prioritized-items (reverse prioritized-items))
  (while prioritized-items
    (let ((i (pop prioritized-items)))
      (when (member i l)
        (setq l (cons i (delete i l))))))
  l)

(defalias '/prioritize 'pg-util-prioritize)

;;;###autoload
(defun pg-util-minor-mode-active-p (minor-mode)
  "Return t if the minor mode is active in the current buffer,
otherwise nil."
  (condition-case nil
      (and (symbolp minor-mode) (symbol-value minor-mode))
    ('error nil)))

(defalias '/minor-mode-active-p 'pg-util-minor-mode-active-p)

;;;###autoload
(defun pg-util-nuke-kill-ring ()
  "Try to annihilate all history of anything recently typed,
copied or pasted."
  (setq kill-ring nil)
  (setcdr yank-menu nil)
  (clear-this-command-keys)
  ;; Things get ugly with various histories, but try a little
  (setq minibuffer-history '()))

(defalias '/nuke-kill-ring 'pg-util-nuke-kill-ring)

;;;###autoload
(defmacro pg-util-diminish-major (mode new-name)
      "Simulate the effects of diminish on major modes."
      `(add-hook
        (quote,(intern (format "%s-hook" (symbol-name mode))))
        (lambda () (setq mode-name ,new-name))))

(defalias '/diminish-major 'pg-util-diminish-major)

(defun pg-util--library-name-at-point ()
  (let* ((dirs (or find-function-source-path load-path))
          (suffixes (find-library-suffixes))
          (table (apply-partially 'locate-file-completion-table
                                  dirs suffixes))
          (def (thing-at-point 'symbol)))
     (when (and def (not (test-completion def table)))
       (setq def nil))
     def))

(defalias '/-library-name-at-point 'pg-util--library-name-at-point)

(defun pg-util--function-name-at-point ()
  "Return the name of the function at point, or nil if point is
not on a function name. (Contrast with `function-at-point', which assumes there's a function around somewhere and tries to find it. This just tells you if point is on a function, and if so which one.)"
  (let ((symb (thing-at-point 'symbol)))
    (if (functionp (intern symb))
        symb
      nil)))

(defalias '/-function-name-at-point 'pg-util--function-name-at-point)


(defun pg-util--variable-name-at-point ()
  "Return variable name at point, or nil if there is none."
  (let ((v (variable-at-point)))
    (if (equal 0 v) nil v)))

(defalias '/-variable-name-at-point 'pg-util--variable-name-at-point)

;;;###autoload
(defun pg-util-find-thing-at-point ()
  "Find the library, function, or variable (in that order) at
point, if it exists."
  (interactive)
  (let* ((symbcell (list (pg-util--library-name-at-point) 'lib))
         (symbcell (if (car symbcell)
                  symbcell
                (list (pg-util--function-name-at-point) 'fun)))
         (symbcell (if (car symbcell)
                  symbcell
                (list (pg-util--variable-name-at-point) 'var)))
         (symb (car symbcell))
         (symbtype (if symb
                      (cadr symbcell)
                    nil)))
    (case symbtype
      ('lib
       (find-library symb))
      ('fun
       (find-function (intern symb)))
      ('var
       (find-variable symb))
      (t (message "Can't ID symbol at point: %s" (thing-at-point 'symbol))))))

(defalias '/find-thing-at-point 'pg-util-find-thing-at-point)

(defmacro pg-util-if-2 (l r)
  "A compact way of saying (if l l r)"
  (declare (indent 1))
  `(if ,l
       ,l
     ,r))

(defalias '/if-2 'pg-util-if-2)


(defmacro pg-util-option (l r)
  "Return L if it evaluates true, otherwise R. L is evaluated
exactly once. R is evaluated only if L evaluates to nil."
  (declare (indent 1))
  `(let ((l ,l))
     (if l
         l
       ,r)))

(defalias '/option 'pg-util-option)

(provide 'pg-util)
;;; pg-util.el ends here
