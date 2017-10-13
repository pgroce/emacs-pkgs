;;; pg-solarized-theme.el --- Custom theme

;; Copyright (C) 2017 Phil Groce

;; Author: Phil Groce <pgroce@gmail.com>
;; Version: 0.1
;; Package-Requires: (solarized-theme)
;; Keywords: utility

(require 'solarized)

(defun pg-solarized-theme-create (lightdark theme)
  "General function for creating a solarized-derived theme."
  (let ((solarized-scale-org-headlines nil)
        (solarized-use-variable-pitch nil)
        (solarized-use-more-italic t))
    (create-solarized-theme
     lightdark
     theme
     (lambda ()
       ;(message (format "class is %s" class))
       (custom-theme-set-faces
        theme
        ;; Note: Commented-out styles were in my old config, but have
        ;; defaults set by solarized that I want to live with for a while.

        ;; diff
        `(diff-hunk-header ((,class (:foreground ,base3 :background ,violet))))
        `(diff-nonexistent ((,class (:foreground ,blue))))
        `(diff-removed ((,class (:foreground ,base1))))

        ;; dired+(--)
        `(diredp-dir-heading ((,class
                               (:foreground ,yellow :slant italic))))
        `(diredp-read-priv ((,class
                             (:foreground ,base01 :background ,base02))))
        `(diredp-write-priv ((,class
                              (:foreground ,base01 :background ,base02))))
        `(diredp-exec-priv ((,class
                             (:foreground ,base01 :background ,base02))))
        `(diredp-dir-priv ((,class
                            (:foreground ,blue :background ,base02 ))))
        `(diredp-link-priv ((,class (:foreground ,cyan, :background ,base02))))
        `(diredp-no-priv ((,class
                             (:background ,base01  :background ,base03))))
        `(diredp-file-name ((,class (:foreground ,base0))))
        `(diredp-file-suffix ((,class (:foreground ,base0))))
        `(diredp-compressed-file-suffix ((,class (:foreground ,blue-d))))
        `(diredp-ignored-file-name ((,class (:foreground ,base01))))
        `(diredp-date-time ((,class (:foreground ,base0))))
        `(diredp-number ((,class (:foreground ,base0))))
        `(diredp-symlink ((,class (:foreground ,cyan :slant italic)))))))))

(deftheme pg-solarized-dark
  "Phil's custom version of solarized-dark")

(pg-solarized-create-solarized-theme 'dark 'pg-solarized-dark)

(provide-theme 'pg-solarized-dark)

(deftheme pg-solarized-light
  "Phil's custom version of solarized-light")

(pg-solarized-create-solarized-theme 'light 'pg-solarized-light)

(provide-theme 'pg-solarized-light)

(provide 'pg-solarized-theme)
;;; pg-solarized-theme.el ends here
