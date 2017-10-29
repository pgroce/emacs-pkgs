;;; pg-s.el --- String functions not in s.el

;; Copyright (C) 2017 Phil Groce

;; Author: Phil Groce <pgroce@gmail.com>
;; Version: 0.1
;; Package-Requires: ((s "1.12.0"))
;; Keywords: string

(require 's)

(defun pg-s-except-left (num-chars s)
  "Returns everything after the first NUM-CHARS characters of S.
This function is the inverse of `s-left'."
  (let ((to-trim (abs (- num-chars (string-width s)))))
    (s-right to-trim s)))

(ert-deftest pg-s-except-left-test ()
  (should (string=
           "cde"
           (pg-s-except-left 2 "abcde")))
  (should (string=
           "abcde"
           (pg-s-except-left 0 "abcde")))
  (should (string=
           ""
           (pg-s-except-left 2 ""))))

(defun pg-s-except-right (num-chars s)
  "Returns everything until the last NUM-CHARS characters of S.
This function is the inverse of `s-right'"
  (let ((to-trim (abs (- num-chars (string-width s)))))
    (s-left to-trim s)))

(ert-deftest pg-s-except-right-test ()
  (should (string=
           "abc"
           (pg-s-except-right 2 "abcde")))
  (should (string=
           "abcde"
           (pg-s-except-right 0 "abcde")))
  (should (string=
           ""
           (pg-s-except-right 2 ""))))

(defun pg-s--halves (len)
   "Divide LEN in two. Allocate the remainer on odd strings to
   the right half. Return a list '(left-len right-len)"
   (let ((half (/ len 2)))
     (if (evenp len)
         (list half half)
       (list half (+ 1 half)))))

(defun pg-s-truncate-middle (str len &optional fill-str)
   (let* ((fill-str (if fill-str fill-str "..."))
          (fill-len (string-width fill-str)))
     (cond
      ;; If the string is shorter than the max length, just return it
      ((<= (string-width str) len) str)
      ;; Ignore negative vales of len
      ((< len 0) str)
      ;; If the length is 0, return empty string
      ((= len 0) "")
      ;; Return first character if length is 1
      ((= len 1) (s-left 1 str))
      ;; Return first and last character if length is 2
      ((= len 2) (concat (s-left 1 str) (s-right 1 str)))
      ;; If the desired length is less than the fill-string length
      ;; plus two characters, (1) wtf, and (2) take characters off the
      ;; front and back of the fill until it is
      ((< len (+ 2 fill-len))
       (let* ((target-fill-len (- len 2))
              ;; The difference between the target length and the fill
              ;; length (IOW, what we have to cut from fill-len)
              (gap (abs (- target-fill-len fill-len))))
         (cl-destructuring-bind (fl fr) (pg-s--halves gap)
           (message "taking left %s and right %s of %s"
                    fl fr fill-str)
           (setq fill-str
                 (pg-s-except-left fl (pg-s-except-right fr fill-str)))
           ;; Note: fill-len is wrong at this point, but it doesn't
           ;; matter.
           (concat (s-trim-right (s-left 1 str))
                   fill-str
                   (s-trim-left (s-right 1 str))))))
      ;; Base case
      (t
       (cl-destructuring-bind (l r) (pg-s--halves (- len fill-len))
         (concat
          (s-trim-right (s-left l str))
          fill-str
          (s-trim-left (s-right r str))))))))

(ert-deftest pg-s-truncate-middle-test ()
  (let ((9-str "abcdefghi"))
    (should (=
             8
             (string-width
              (pg-s-truncate-middle 9-str 8))))
    (should (string-equal
             ""
             (pg-s-truncate-middle "" 8)))
    (should (string-equal
             "abcdefghi"
             (pg-s-truncate-middle 9-str -1)))
    (should (string-equal
             ""
             (pg-s-truncate-middle 9-str 0)))
    (should (string-equal
             "a"
             (pg-s-truncate-middle 9-str 1)))
    (should (string-equal
             "ai"
             (pg-s-truncate-middle 9-str 2)))
    (should (string-equal
             "a.i"
             (pg-s-truncate-middle 9-str 3)))
    (should (string-equal
             "a..i"
             (pg-s-truncate-middle 9-str 4)))
    (should (string-equal
             "a...i"
             (pg-s-truncate-middle 9-str 5)))
    (should (string-equal
             "a...hi"
             (pg-s-truncate-middle 9-str 6)))
    (should (string-equal
             "ab...hi"
             (pg-s-truncate-middle 9-str 7)))
    (should (string-equal
             "ab...ghi"
             (pg-s-truncate-middle 9-str 8)))
    (should (string-equal
             "abcdefghi"
             (pg-s-truncate-middle 9-str 9)))
    (should (string-equal
             "abcdefghi"
             (pg-s-truncate-middle 9-str 10)))))

(provide 'pg-s)
;;; pg-s.el ends here
