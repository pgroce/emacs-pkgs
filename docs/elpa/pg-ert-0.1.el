;;; pg-ert.el --- extensions to ert

;; Copyright (C) 2021 Phil Groce

;; Author: Phil Groce <pgroce@gmail.com>
;; Version: 0.1
;; Package-Requires: ((dash "2.13.0"))
;; Keywords: utility

(require 'ert)

(defun pg-ert-run-tests-string (selector &optional return-stats)
  "Run ERT tests and report the output in a string. Returns the
string; if RETURN-STATS is non-nil, instead returns a cons cell
of the string (car) and the `ert--stats` object (cdr).''"
  (unless selector (error "selector is required"))
  (cl-macrolet ((->acc (fstr &rest args)
                       `(setq acc (append acc (list (format ,fstr ,@args))))))
    (let* ((acc nil)
           (stats
            (ert-run-tests
             selector
             (lambda (event-type &rest event-args)
               (cl-ecase event-type
                 (run-started
                  (unless ert-quiet
                    (cl-destructuring-bind (stats) event-args
                      (progn
                        (message "acc is %s" acc)
                        (append acc
                                (list
                                 (format "Running %s tests (%s, selector `%S')"
                                         (length (ert--stats-tests stats))
                                         (ert--format-time-iso8601
                                          (ert--stats-start-time stats))
                                         selector)))))))
                 (run-ended
                  (cl-destructuring-bind (stats abortedp) event-args
                    (let ((unexpected (ert-stats-completed-unexpected stats))
                          (skipped (ert-stats-skipped stats))
                          (expected-failures (ert--stats-failed-expected stats)))
                      (->acc
                       (concat "\n%sRan %s tests, %s results as expected,"
                               " %s unexpected%s (%s, %f sec)%s\n")
                       (if (not abortedp) "" "Aborted: ")
                       (ert-stats-total stats)
                       (ert-stats-completed-expected stats)
                       unexpected
                       (if (zerop skipped)
                           ""
                         (format ", %s skipped" skipped))
                       (ert--format-time-iso8601 (ert--stats-end-time stats))
                       (float-time
                        (time-subtract
                         (ert--stats-end-time stats)
                         (ert--stats-start-time stats)))
                       (if (zerop expected-failures)
                           ""
                         (format "\n%s expected failures" expected-failures)))
                      (unless (zerop unexpected)
                        (->acc "%s unexpected results:" unexpected)
                        (cl-loop
                         for test across (ert--stats-tests stats)
                         for result = (ert-test-most-recent-result test) do
                         (when (not (ert-test-result-expected-p test result))
                           (->acc "%9s  %S"
                                       (ert-string-for-test-result result nil)
                                       (ert-test-name test))))
                        (->acc "%s" ""))
                      (unless (zerop skipped)
                        (->acc "%s skipped results:" skipped)
                        (cl-loop for test across (ert--stats-tests stats)
                                 for result = (ert-test-most-recent-result test) do
                                 (when (ert-test-result-type-p result :skipped)
                                   (->acc
                                    "%9s  %S"
                                    (ert-string-for-test-result result nil)
                                    (ert-test-name test))))
                        (->acc "%s" "")))))
                 (test-started
                  )
                 (test-ended
                  (cl-destructuring-bind (stats test result) event-args
                    (unless (ert-test-result-expected-p test result)
                      (cl-etypecase result
                        (ert-test-passed
                         (->acc
                          "Test %S passed unexpectedly"
                          (ert-test-name test)))
                        (ert-test-result-with-condition
                         (->acc
                          "Test %S backtrace:"
                          (ert-test-name test))
                         (with-temp-buffer
                           (insert
                            (backtrace-to-string
                             (ert-test-result-with-condition-backtrace result)))
                           (if (not ert-batch-backtrace-right-margin)
                               (->acc
                                "%s"
                                (buffer-substring-no-properties
                                 (point-min)
                                 (point-max)))
                             (goto-char (point-min))
                             (while (not (eobp))
                               (let ((start (point))
                                     (end (line-end-position)))
                                 (setq end (min
                                            end
                                            (+ start
                                               ert-batch-backtrace-right-margin)))
                                 (->acc
                                  "%s"
                                  (buffer-substring-no-properties
                                   start end)))
                               (forward-line 1))))
                         (with-temp-buffer
                           (ert--insert-infos result)
                           (insert "    ")
                           (let ((print-escape-newlines t)
                                 (print-level 5)
                                 (print-length 10))
                             (ert--pp-with-indentation-and-newline
                              (ert-test-result-with-condition-condition result)))
                           (goto-char (1- (point-max)))
                           (cl-assert (looking-at "\n"))
                           (delete-char 1)
                           (->acc
                            "Test %S condition:"
                            (ert-test-name test))
                           (->acc
                            "%s"
                            (buffer-string))))
                        (ert-test-aborted-with-non-local-exit
                         (->acc
                          "Test %S aborted with non-local exit"
                          (ert-test-name test)))
                        (ert-test-quit
                         (->acc
                          "Quit during %S"
                          (ert-test-name test)))))
                    (unless ert-quiet
                      (let* ((max (prin1-to-string (length (ert--stats-tests stats))))
                             (format-string (concat "%9s  %"
                                                    (prin1-to-string (length max))
                                                    "s/" max "  %S (%f sec)")))
                        (->acc
                         format-string
                         (ert-string-for-test-result result
                                                     (ert-test-result-expected-p
                                                      test result))
                         (1+ (ert--stats-test-pos stats test))
                         (ert-test-name test)
                         (ert-test-result-duration result))))))))
             nil))
           (acc-str (s-join "\n" acc)))
      (if return-stats
          (cons acc-str stats)
        acc-str))))



(defun pg-ert-run-tests-string-OLD (selector &optional return-stats)
  "Run ERT tests and report the output in a string. Returns the
string; if RETURN-STATS is non-nil, instead returns a cons cell
of the string (car) and the `ert--stats` object (cdr).''"
  (unless selector (error "selector is required"))
  (cl-macrolet ((accumulate (expr) `(setq acc (append acc (list ,expr)))))
    (let* ((acc nil)
           (stats
            (ert-run-tests
             selector
             (lambda (event-type &rest event-args)
               (cl-ecase event-type
                 (run-started
                  (unless ert-quiet
                    (cl-destructuring-bind (stats) event-args
                      (progn
                        (message "acc is %s" acc)
                        (append acc
                                (list
                                 (format "Running %s tests (%s, selector `%S')"
                                         (length (ert--stats-tests stats))
                                         (ert--format-time-iso8601
                                          (ert--stats-start-time stats))
                                         selector)))))))
                 (run-ended
                  (cl-destructuring-bind (stats abortedp) event-args
                    (let ((unexpected (ert-stats-completed-unexpected stats))
                          (skipped (ert-stats-skipped stats))
                          (expected-failures (ert--stats-failed-expected stats)))
                      (accumulate
                       (format
                        "\n%sRan %s tests, %s results as expected, %s unexpected%s (%s, %f sec)%s\n"
                        (if (not abortedp)
                            ""
                          "Aborted: ")
                        (ert-stats-total stats)
                        (ert-stats-completed-expected stats)
                        unexpected
                        (if (zerop skipped)
                            ""
                          (format ", %s skipped" skipped))
                        (ert--format-time-iso8601 (ert--stats-end-time stats))
                        (float-time
                         (time-subtract
                          (ert--stats-end-time stats)
                          (ert--stats-start-time stats)))
                        (if (zerop expected-failures)
                            ""
                          (format "\n%s expected failures" expected-failures))))
                      (unless (zerop unexpected)
                        (accumulate (format "%s unexpected results:" unexpected))
                        (cl-loop
                         for test across (ert--stats-tests stats)
                         for result = (ert-test-most-recent-result test) do
                         (when (not (ert-test-result-expected-p test result))
                           (accumulate (format "%9s  %S"
                                               (ert-string-for-test-result result nil)
                                               (ert-test-name test)))))
                        (accumulate (format "%s" "")))
                      (unless (zerop skipped)
                        (accumulate (format "%s skipped results:" skipped))
                        (cl-loop for test across (ert--stats-tests stats)
                                 for result = (ert-test-most-recent-result test) do
                                 (when (ert-test-result-type-p result :skipped)
                                   (accumulate
                                    (format "%9s  %S"
                                            (ert-string-for-test-result result nil)
                                                       (ert-test-name test)))))
                        (accumulate (format "%s" ""))))))
                 (test-started
                  )
                 (test-ended
                  (cl-destructuring-bind (stats test result) event-args
                    (unless (ert-test-result-expected-p test result)
                      (cl-etypecase result
                        (ert-test-passed
                         (accumulate
                          (format
                           "Test %S passed unexpectedly"
                           (ert-test-name test))))
                        (ert-test-result-with-condition
                         (accumulate
                          (format
                           "Test %S backtrace:"
                           (ert-test-name test)))
                         (with-temp-buffer
                           (insert
                            (backtrace-to-string
                             (ert-test-result-with-condition-backtrace result)))
                           (if (not ert-batch-backtrace-right-margin)
                               (accumulate
                                (format
                                 "%s"
                                 (buffer-substring-no-properties
                                  (point-min)
                                  (point-max))))
                             (goto-char (point-min))
                             (while (not (eobp))
                               (let ((start (point))
                                     (end (line-end-position)))
                                 (setq end (min
                                            end
                                            (+ start
                                               ert-batch-backtrace-right-margin)))
                                 (accumulate
                                  (format
                                   "%s"
                                   (buffer-substring-no-properties
                                    start end))))
                               (forward-line 1))))
                         (with-temp-buffer
                           (ert--insert-infos result)
                           (insert "    ")
                           (let ((print-escape-newlines t)
                                 (print-level 5)
                                 (print-length 10))
                             (ert--pp-with-indentation-and-newline
                              (ert-test-result-with-condition-condition result)))
                           (goto-char (1- (point-max)))
                           (cl-assert (looking-at "\n"))
                           (delete-char 1)
                           (accumulate
                            (format
                             "Test %S condition:"
                             (ert-test-name test)))
                           (accumulate
                            (format
                             "%s"
                             (buffer-string)))))
                        (ert-test-aborted-with-non-local-exit
                         (accumulate
                          (format
                           "Test %S aborted with non-local exit"
                           (ert-test-name test))))
                        (ert-test-quit
                         (accumulate
                          (format
                           "Quit during %S"
                           (ert-test-name test))))))
                    (unless ert-quiet
                      (let* ((max (prin1-to-string (length (ert--stats-tests stats))))
                             (format-string (concat "%9s  %"
                                                    (prin1-to-string (length max))
                                                    "s/" max "  %S (%f sec)")))
                        (accumulate
                         (format
                          format-string
                          (ert-string-for-test-result result
                                                      (ert-test-result-expected-p
                                                                         test result))
                                            (1+ (ert--stats-test-pos stats test))
                                            (ert-test-name test)
                                            (ert-test-result-duration result)))))))))
             nil))
           (acc-str (s-join "\n" acc)))
      (if return-stats
          (cons acc-str stats)
        acc-str))))

(provide 'pg-ert)
;;; pg-ert.el ends here
