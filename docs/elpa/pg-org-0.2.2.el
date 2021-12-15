;;; pg-org.el --- Customizations to org-mode

;; Copyright (C) 2021 Phil Groce

;; Author: Phil Groce <pgroce@gmail.com>
;; Version: 0.2.2
;; Package-Requires: ((emacs "26.1") (org-ml "5.7") (dash "2.19") (s "1.12") (ts "0.3") (pg-ert "0.1"))
;; Keywords: productivity

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

(defun pg-org-headline-logbook-entries (headline)
  "Given a headline org element, return its logbook entries as a
  list of paragraph elements. If the headline doesn't contain any
  logbook entries, return `nil'."
  (->> headline
       (org-ml-match
        '(section
          (:and drawer (:drawer-name "LOGBOOK"))
          plain-list
          item
          paragraph))

       ))

(defun pg-org-lookahead (match-criteria)
  "Return a function that takes an org-element node and runs
  `org-ml-match' on it using MATCH-CRITERIA as the match
  criteria. Returns a true value if the match returns results,
  else `nil'."
  (-partial #'org-ml-match match-criteria))

(provide 'pg-org)
;;; pg-org.el ends here
