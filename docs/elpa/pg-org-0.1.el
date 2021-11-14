;;; pg-org.el --- Customizations to org-mode

;; Copyright (C) 2021 Phil Groce

;; Author: Phil Groce <pgroce@gmail.com>
;; Version: 0.1
;; Package-Requires: ((emacs "26.1") (org-ml "5.7"))
;; Keywords: productivity

(require 'org-ml)

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

(provide 'pg-org)
;;; pg-org.el ends here
