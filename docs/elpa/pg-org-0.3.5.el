;;; pg-org.el --- Customizations to org-mode

;; Copyright (C) 2021 Phil Groce

;; Author: Phil Groce <pgroce@gmail.com>
;; Version: 0.3.5
;; Package-Requires: ((emacs "26.1") (org-ml "5.7") (dash "2.19") (s "1.12") (ts "0.3") (pg-ert "0.1"))
;; Keywords: productivity

(eval-when-compile (require 'cl-macs))
(require 'org-ml)
(require 'dash)
(require 's)
(require 'ts)

(defmacro pg-org-with-src (block-name &rest body)
  "Put the text in the source block BLOCK-NAME in a temp buffer,
then execute BODY in that buffer."
  (declare (indent 1))
  (let ((-src (make-symbol "-src")))
    `(cl-flet ((-assert (msg arg) (if (eq nil arg) (error msg) arg)))
       (let ((,-src

              (->> (org-ml-parse-this-buffer)
                   (org-ml-match
                    '(:any * (:and
                              src-block
                              (:name ,(symbol-name block-name)))))
                   (-assert (format "No src block %s" ,(symbol-name block-name)))
                   (car)
                   (org-ml-get-property :value))))
         ;;  Put the source block in a separate buffer and run the code in body
         (with-temp-buffer
           (insert ,-src)
           ,@body)))))


(defmacro pg-org-with-src-doc (block-name &rest body)
  "Parse the text in the org-mode source block BLOCK-NAME into an
  org-element tree and run BODY. Code in BODY can refer to the
  org-element tree via the symbol `doc'."
  (declare (indent 1))
  `(lexical-let ((doc (pg-org-with-src ,block-name
                        (org-unescape-code-in-region (point-min) (point-max))
                        (org-do-remove-indentation)
                        (org-mode)
                        (org-ml-parse-this-buffer))))
     ,@body))

(defmacro pg-org-deftest (test-name block-name &rest body)
  "Use `pg-org-with-src-doc' to parse BLOCK-NAME into an
org-element tree, then define an ERT test named TEST-NAME (using
`ert-deftest') whose body is BODY."
  (declare (indent 2))
  `(pg-org-with-src-doc ,block-name
     (ert-deftest ,test-name () ,@body)))

(defun org-ml-normalize (tree)
  "Convert TREE into an org-ml build invocation, by prepending
  \"org-ml-build-\" to the symbol in first position of the
  list. This function is then applied recursively to all the tree
  node's children. (In this case, any lists contained in TREE
  whose first element is a symbol.) "
  ;; Only handle lists whose first element is a symbol
  (if (symbolp (car tree))
      ;; Normalize the root node
      (->> (cons (->> (car tree)
                      (symbol-name)
                      (concat "org-ml-build-")
                      (intern))
                 (cdr tree))
           ;; ...and its children
           (--map (if (listp it) (org-ml-normalize it) it)))
    tree))

(defun org-ml-build (spec)
  "Transform SPEC into an org-element tree using constructors for
elements in `org-ml'.

All that is done to transform SPEC is that the first element of
every list is prepended with \"org-ml-build-\" if it is a
symbol. SPEC's format, then, is that of a tree of lists whose
first elements are symbols representing element types; the rest
of the elements are the arguments used to construct an element
type using org-ml's corresponding \"org-ml-build-*\"
corresponding to that symbol. A SPEC for a headline element, for
instance, might be:

  (headline :title (secondary-string! \"foo\")
    (section (paragraph! \"paragraph text\")))

This function will convert that specification into the result of
calling:

  (org-ml-build-headline
    :title (org-ml-build-secondary-string! \"foo\")
    (org-ml-build-section
      (org-ml-build-paragraph! \"paragraph text\")))"
  (eval (org-ml-normalize spec)))

(defcustom pg-org-headline-logging-config
  '(:log-into-drawer "LOGBOOK" :clock-into-drawer t)
  "Default logging format for Org files."
  :type '(plist)
  :group 'pm)


;; Supercontents

(defun pg-org-headline-get-supercontents (headline)
  "Use `org-ml-headline-get-supercontents' to return the
  supercontents of HEADLINE. Uses
  `pg-org-headline-logging-config' for logging configuration
  information."
  (org-ml-headline-get-supercontents
   pg-org-headline-logging-config headline))

(defun pg-org-headline-set-supercontents (supercontents headline)
  "Use `org-ml-headline-set-supercontents' to set the
  supercontents of HEADLINE. Uses
  `pg-org-headline-logging-config' for logging configuration
  information."
  (org-ml-headline-set-supercontents
   pg-org-headline-logging-config supercontents headline))

(defun pg-org-headline-map-supercontents (fun headline)
  "Use `org-ml-headline-map-supercontents' to map the
  supercontents of HEADLINE. Uses
  `pg-org-headline-logging-config' for logging configuration
  information."
  (org-ml-headline-map-supercontents
      pg-org-headline-logging-config fun headline))


;; Logbook items

(defun pg-org-headline-get-logbook-items (headline)
  "Use `org-ml-headline-get-logbook-items' to pull logbook items
  off HEADLINE. Uses `pg-org-headline-logging-config' for logging
  configuration information."
  (org-ml-headline-get-logbook-items
   pg-org-headline-logging-config
   headline))

(defun pg-org-headline-set-logbook-items (items headline)
  "Use `org-ml-headline-set-logbook-items' to set logbook items
  for HEADLINE. Uses `pg-org-headline-logging-config' for logging
  configuration information."
  (org-ml-headline-set-logbook-items
   pg-org-headline-logging-config
   items
   headline))

(defun pg-org-headline-map-logbook-items (fun headline)
  "Use `org-ml-headline-map-logbook-items' to set logbook items
  for HEADLINE. Uses `pg-org-headline-logging-config' for logging
  configuration information."
  (org-ml-headline-map-logbook-items
   pg-org-headline-logging-config
   fun
   headline))


;; Logbook clocks

(defun pg-org-headline-get-logbook-clocks (headline)
  "Use `org-ml-headline-get-logbook-clocks' to pull logbook clocks
  off HEADLINE. Uses `pg-org-headline-logging-config' for logging
  configuration information."
  (org-ml-headline-get-logbook-clocks
   pg-org-headline-logging-config
   headline))

(defun pg-org-headline-set-logbook-clocks (clocks headline)
  "Use `org-ml-headline-set-logbook-clocks' to set logbook clocks
  for HEADLINE. Uses `pg-org-headline-logging-config' for logging
  configuration information."
  (org-ml-headline-set-logbook-clocks
   pg-org-headline-logging-config
   clocks
   headline))

(defun pg-org-headline-map-logbook-clocks (fun headline)
  "Use `org-ml-headline-map-logbook-clocks' to set logbook clocks
  for HEADLINE. Uses `pg-org-headline-logging-config' for logging
  configuration information."
  (org-ml-headline-map-logbook-clocks
   pg-org-headline-logging-config
   fun
   headline))



;; Contents


(defun pg-org-headline-get-contents (headline)
  "Use `org-ml-headline-get-contents' to return the contents of
  HEADLINE. Uses `pg-org-headline-logging-config' for logging
  configuration information."
  (org-ml-headline-get-contents
   pg-org-headline-logging-config headline))

(defun pg-org-headline-set-contents (contents headline)
  "Use `org-ml-headline-set-contents' to set the contents of
  HEADLINE. Uses `pg-org-headline-logging-config' for logging
  configuration information."
  (org-ml-headline-set-contents
   pg-org-headline-logging-config contents headline))

(defun pg-org-headline-map-contents (fun headline)
  "Use `org-ml-headline-map-contents' to map the contents of
  HEADLINE. Uses `pg-org-headline-logging-config' for logging
  configuration information."
  (org-ml-headline-map-contents
      pg-org-headline-logging-config fun headline))

;; Other logbook

(defun pg-org-headline-logbook-append-item (item headline)
  "Use `org-ml-headline-append-item' to return the contents
  of HEADLINE. Uses `pg-org-headline-logging-config' for logging
  configuration information."
  (org-ml-headline-logbook-append-item
   pg-org-headline-logging-config item headline))

(defun pg-org-headline-logbook-append-open-clock (unixtime note headline)
  "Use `org-ml-headline-logbook-append-open-clock' to add an open
  clock into the logbook of HEADLINE. Uses
  `pg-org-headline-logging-config' for logging configuration
  information."
  (org-ml-headline-logbook-append-open-clock
   pg-org-headline-logging-config unixtime headline))

(defun pg-org-headline-logbook-close-open-clock (unixtime note headline)
  "Use `org-ml-headline-logbook-close-open-clock' to close an
  open clock in the logbook of HEADLINE. Uses
  `pg-org-headline-logging-config' for logging configuration
  information."
  (org-ml-headline-logbook-close-open-clock
   pg-org-headline-logging-config unixtime note headline))

(defun pg-org-headline-logbook-entries (headline)
  "Given a headline org element, return its logbook entries as a
list of paragraph elements. If the headline doesn't contain any
logbook entries, return `nil'."
  (let ((drawer-name (plist-get
                      pg-org-headline-logging-config
                      :log-into-drawer)))
    (->> headline
         (org-ml-match
          '(section
            (:and drawer (:drawer-name drawer-name))
            plain-list
            item
            paragraph)))))

(defcustom pg-org--rx-logbook-status-change
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
  :group 'pg-org)

(defun pg-org-paragraph-parse-status-change (para)
  "If PARA is a logbook entry that looks like it was generated
  when a to-do item's status changed, parse it and return a list of
  the state it was changed to (as a string), the state it was
  changed from (as a string), the timestamp, and an org paragraph
  element representing any additional notes provided by the
  user. Otherwise, return nil."
  (-when-let* [((_ _ s ts . the-rest)  para)
               ;; parse out the to and from states
               ((_ from to) (->> (org-ml-to-trimmed-string s)
                                 (s-match pg-org--rx-logbook-status-change)))
               ;; if notes exist, create as new paragraph
               (notes (->> (if (org-ml-is-type 'line-break (first the-rest))
                               ;; trick to inline (cdr the-rest) as args
                               (let ((para-objs (-map (lambda (x) `(quote ,x)) (cdr the-rest))))
                                 (eval `(org-ml-build-paragraph ,@para-objs)))
                             ;; no additional notes == empty paragraph
                             (org-ml-build-paragraph))
                           (org-ml-remove-parents)))]
    (list to from (org-ml-remove-parents ts) notes)))

(defun pg-org-lookahead (match-criteria)
  "Return a function that takes an org-element node and runs
  `org-ml-match' on it using MATCH-CRITERIA as the match
  criteria. Returns a true value if the match returns results,
  else `nil'."
  (-partial #'org-ml-match match-criteria))

(defun pg-org--match-build-pattern (pattern)
  ;; Make this (-let (...) (case ...)) into a (pcase ...)?
  (-let (((tok . rest) pattern))
    (cl-case tok
      ;; The patterns we transform:
      ;; - :lookahead
      (:lookahead
       (progn
         (-let* ((sym (gensym "lookahead-"))
                 (clause
                  `((symbol-function (quote ,sym))
                    (lambda (el) (pg-org-match (quote ,(car rest)) el))))
                 (new-pattern `(:pred ,sym)))
           (list (list clause) new-pattern))))
      ;; - :-pred
      (:-pred
       (progn
         (-let* ((sym (gensym "pred-"))
                 (clause
                  `((symbol-function (quote ,sym))
                    (lambda (el) ,(car rest))))
                 (new-pattern `(:pred ,sym)))
           (list (list clause) new-pattern))))
      ;; unary prefixes; leave them unchanged and consume rest of the list
      ((:first :last :and :or :not)
       (progn
         (-let (((clauses rest-pattern) (pg-org--match-build-pattern rest)))
           (list clauses (cons tok rest-pattern)))))
      ;; 2-ary prefixes
      (:nth
       (progn
         (-let* (((x . rest) rest)
                 ((clauses rest-pattern) (pg-org--match-build-pattern rest)))
           (list clauses (-concat `(,tok ,x) rest-pattern)))))
      ;; 3-ary prefixes
      (:sub
       (progn
         (-let* (((x y . rest) rest)
                 ((clauses rest-pattern) (pg-org--match-build-pattern rest)))
           (list clauses (-concat `(,tok ,x ,y) rest-pattern)))))
      ;; general case – if it's a list, modify it and consume the rest
      ;; of the list. If it's a symbol we don't need to modify, yield
      ;; it unchanged and consume the rest of the list.
      (t
       (cond
        ((listp tok)
         ;; Subpattern; get the clauses and new pattern associated
         ;; with it, and combine with the rest of the "horizontal"
         ;; pattern
         (progn
           (cond
            ;; base case
            ((eq nil tok)
             '(nil nil))
            ;; descend into list
            (t
             (-let* (((cl1 p1) (pg-org--match-build-pattern tok))
                     ((cl2 p2) (pg-org--match-build-pattern rest))
                     (new-clauses (-concat cl1 cl2))
                     (new-pattern (cons p1 p2)))
               (list new-clauses new-pattern))))))

        ((symbolp tok)
         (cond
          ;; Property name (or any other special form org-ml-match
          ;; handles)
          ((s-starts-with? ":" (symbol-name tok))
           (progn
             (message "[symbol] TOK: %s" tok)
             (message "[symbol] REST: %s" rest)
             (list nil `(,tok ,@rest))))
          ;; Element name
          (t
           (progn
             (-let (((clauses pattern) (pg-org--match-build-pattern rest)))
               (list clauses (cons tok pattern)))))))))
      )))

(defmacro pg-org-match (pattern node)
    "Match PATTERN against NODE, in the form of `org-ml-match', but with a more powerful extended syntax.

  `pg-org-match' supports the following additional match patterns:
()
  `(:lookahead SUBPATTERN)' runs a second `org-ml-match' on the
  children of the current node, returning a true value if
  SUBPATTERN matches any of the node's children. In other words, it
  matches nodes based on the properties of the nodes' children. In
  this way, one can, say, match headlines with a LOGBOOK drawer
  with the following pattern:

    (:and headline
          (:lookahead (section (:and drawer
                                     (:drawer-name \"LOGBOOK\")))))

  `(:-pred CODE)' implements an anaphoric predicate. CODE is
  interpreted as the body of a lambda expression, which is called
  on a node using `(:pred ...)'. CODE may refer to the variable
  `el', which is the element currently being considered. Thus, the
  following code block:

    (cl-letf ((fn (lambda (el)
                     (org-ml-headline-has-tag \"work\" el))))
      (org-ml-match '((:pred fn)) node))


  Is equivalent to this call to `pg-org-match':

    (pg-org-match '((:-pred (org-ml-headline-has-tag \"work\" el))) node)

  In all other respects, this function is equivalent to a call to
  `org-ml-match'.
  "
    (-let (((clauses new-pattern) (pg-org--match-build-pattern pattern)))
      `(cl-letf ,clauses
         (org-ml-match ,new-pattern ,node))))

(provide 'pg-org)
;;; pg-org.el ends here
