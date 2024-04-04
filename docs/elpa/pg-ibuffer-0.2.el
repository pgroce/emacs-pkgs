;;; pg-ibuffer.el --- Improvements to ibuffer

;; Copyright (C) 2017 Phil Groce

;; Author: Phil Groce <pgroce@gmail.com>
;; Version: 0.2
;; Package-Requires: ((dash "2.13.0"))
;; Keywords: ibuffer

(require 'dash)

;;; Nomenclature
;;
;; ibuffer-saved-filter-groups is a list of filter groups.
;; A filter group = (name divs).
;; A div = (dname qualifier).
;; A qualifier = (name . <str>) | (mode . <mode-sym>)


;; (pg-ibuffer-modify-filter-groups
;;  '("default"
;;    ("python" (mode . python-mode))
;;    ("psp"    (mode . psp)))
;;  '("programming" ("python" (mode . python-mode))))


(defun pg-ibuffer--get-fg (fgs group-name)
  "Return group in FGS named GROUP-NAME.

If GROUP-NAME isn't in FGS, return nil."
  (let ((idx (--find-index (string= group-name (car it)) fgs)))
    (if (eq idx nil)
        nil
      (nth idx fgs))))



(defun pg-ibuffer--merge-filter-groups (old-group new-group)
  "Merge OLD-GROUP and NEW-GROUP to create new filter group.

The new filter group takes the name from NEW-GROUP.

If NEW-GROUP is nil, return OLD-GROUP. If OLD-GROUP is nil,
return NEW-GROUP. If both are nil, return nil."
  (if (eq nil old-group)
      (if (eq nil new-group)
          ;; Both nil, return empty group
          nil
        new-group)
    (if (eq nil new-group)
        old-group
      (let* ((old-divs (cadr old-group))
             (new-divs (cadr new-group))
             ;; Divs , like filter groups, start with the string name of
             ;; the qualifier, so we'll match on that.
             (matcher (lambda (a b) (string= (car a) (car b))))
             (merged-divs (pg-util-list-update
                           old-divs new-divs matcher 'append)))
        (list (car new-group) merged-divs)))))



(defun pg-ibuffer--update-filter-group (fgs mod-group)
  "Return a copy of FGS with MOD-GROUP merged into it.

If MOD-GROUP is nil, return FGS unchanged."
  (if (eq nil mod-group)
      fgs
    (let* ((old-group (pg-ibuffer--get-fg fgs (car mod-group))))
      (if (eq nil old-group)
          ;; Group isn't in fgs, so add it and return it
          (append fgs (list mod-group))
        ;; Merge the filter qualifiers from the group with the ones
        ;; from mod-group and return updated list.
        (let* ((merged-group (pg-ibuffer--merge-filter-groups
                              old-group mod-group))
               (matcher (lambda (a) (string= (car a) (car merged-group)))))
          (pg-util-list-update-1 fgs merged-group matcher))))))


(defun pg-ibuffer--modify-filter-groups (fgs mod-groups)
  "Modify FGS non-destructively using MOD-GROUPS.

This function does the work for `pg-ibuffer-modify-filter-groups'
and `pg-ibuffer-modify-saved-filter-groups'."
  (cl-dolist (m mod-groups fgs)
    (setq fgs (pg-ibuffer--update-filter-group fgs m))))



;;; Public functions


;;;###autoload
(defun pg-ibuffer-modify-filter-groups (fgs &rest mod-groups)
  "Non-destructively modify FGS with MOD-GROUPS.

FGS has the same structure as
`ibuffer-saved-filter-groups'. MOD-GROUPS has the same semantics
as in `pg-ibuffer-modify-saved-filter-groups'. (Calling the
latter function is equivalent to calling this function and
setting `ibuffer-saved-filter-groups' with the output.)

Returns a new copy of FGS, suitably modified."
  (pg-ibuffer--modify-filter-groups fgs mod-groups))

;;;###autoload
(defun pg-ibuffer-modify-saved-filter-groups (&rest mod-groups)
  "Add MOD-GROUPS to ibuffer filter groups.

Each mod-group should be have the following structure:

  (\"STRING\"  QUALIFIERS)

This structure mirrors the structure of `ibuffer-saved-filter-groups'.

The filtering groups specified in this function will be added if
they do not exist. If they do exist, their qualifiers will be
merged into the existing group, replacing any preexisting
qualifiers with the same string identifier."
  (setq ibuffer-saved-filter-groups
        (pg-ibuffer--modify-filter-groups ibuffer-saved-filter-groups
                                          mod-groups)))


(defalias 'pg-ibuffer 'pg-ibuffer-modify-saved-filter-groups)


;;; Cruft for testing TODO: Remove and/or turn into a test file

;; (setq pg-ref-fgs
;;       '(("default"
;;          (("text" (or
;;                     (mode . text-mode)
;;                     (mode . fundamental-mode)
;;                     (mode . visual-line-mode)))
;;           ("Man Pages" (name . "^\\*Man ")))))))

;; (setq pg-ref-abstract-fgs
;;       '(("a"
;;          (("a1" (name . "foo"))
;;           ("a2" (mode . bar)))))))))



;; '(let* ((t1-in '(("a"
;;                   (("a1" (name . "foo"))
;;                    ("a2" (mode . bar))))
;;                  ("b"
;;                   (("b1" (name . "bar")))))))
;;   (pg-ibuffer-modify-filter-groups
;;    t1-in
;;    '("a" (("a1" (name . "changed"))))))

;; '(let* ((t1-in '(("a"
;;                  (("a1" (name . "foo"))
;;                   ("a2" (mode . bar))))
;;                 ("b"
;;                  (("b1" (name . "bar"))))))
;;        (old-group (pg-ibuffer--get-fg t1-in "a"))
;;        (new-group '("a" (("a3" (name . "changed"))))))
;;   (message "%s" (pg-ibuffer--merge-filter-groups old-group new-group))
;;   (message "%s" (pg-ibuffer--merge-filter-groups old-group nil))
;;   (message "%s" (pg-ibuffer--merge-filter-groups nil new-group))
;;   (message "%s" (pg-ibuffer--merge-filter-groups nil nil)))




;; '(let* ((fgs '(("a"
;;                 (("a1" (name . "foo"))
;;                  ("a2" (mode . bar))))
;;               ("b"
;;                (("b1" (name . "bar"))))))
;;        (mod-group '("a" (("a1" (name . "changed"))))))
;;   (message "%s" (pg-ibuffer--update-filter-group fgs mod-group))
;;   (message "%s" (pg-ibuffer--update-filter-group fgs nil)))



;; '(let* ((fgs '(("a"
;;                (("a1" (name . "foo"))
;;                 ("a2" (mode . bar))))
;;               ("b"
;;                (("b1" (name . "bar"))))))
;;        (mod-groups-1 '(("a" (("a1" (name . "changed"))))))
;;        (mod-groups-2 '(("a" (("a1" (name . "changed"))))
;;                        ("b" (("b1" (name . "changed")))))))
;;   (format "%s" (pg-ibuffer--modify-filter-groups fgs mod-groups-2)))

;; '(let* ((fgs '(("a"
;;                 (("a1" (name . "foo"))
;;                  ("a2" (mode . bar))))
;;               ("b"
;;                (("b1" (name . "bar"))))))
;;        (a '("a" (("a1" (name . "changed")))))
;;        (b '("b" (("b1" (name . "changed"))))))
;;   (format "%s" (pg-ibuffer-modify-filter-groups fgs a b)))

(provide 'pg-ibuffer)
;;; pg-ibuffer.el ends here
