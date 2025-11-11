;;; autovenv.el --- Automatically activate Python virtualenvs based on project directory -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Phil Groce

;; Author: pgroce <pgroce@gmail.com>
;; Version: 0.1.7
;; Keywords: python, virtualenv, environment, tools, projects
;; Package-Requires: ((cl-lib "0.5"))
;; License: GPL-3.0-or-later

;;; Commentary:
;;
;;
;; autovenv aims to automatically manage the use of Python virtual
;; environments in Emacs by always making sure the "right" virtual
;; environment is active for the curent buffer.
;;
;; autovenv only tends to the core configuration of the Emacs
;; environment to support virtual environments: Setting the
;; VIRTUAL_ENV environment variable, and adding the relevant bin
;; directory to exec-path and the PATH environment
;; variable. Furthermore, its algorithm for locating virtual
;; environments is very simple. As a result, it has no dependencies
;; that don't ship with Emacs.
;;
;; That said, the user can override the location process to use a
;; different scheme if desired, and hooks around activation and
;; deactivation permit maintenance of additional state if needed, such
;; as reloading LSPs, so autovenv can serve as the core of a workflow
;; that encompasses many other tools, such as Projectile or LSP Mode.
;;
;; This is a fork of auto-virtualenv.el
;; <https://github.com/marcwebbie/auto-virtualenv> by Markwebbie. Most
;; of it has been rewritten, but some of the original code survives,
;; and the template has been very helpful.
;;
;; To use this package, add the `autoenv-find-and-activate' function
;; to hooks associated with changes to virtual
;; environments. `find-file-hook', for instance, is a good candidate,
;; as are hooks for shell buffers that are run when the
;; default-directory changes. If desired, customize the hooks in the
;; `autovenv' Customize group to execute custom behavior on activation
;; or deactivation, or override the location of a virtual environment
;; with a custom fuction.
;;
;; The following use-package declaration illustrates one of the
;; simplest configurations:
;;
;;   (use-package autovenv
;;     :ensure t
;;     :hook ((find-file-hook
;;             eshell-directory-change-hook) . autovenv-find-and-activate))
;;
;;
;;; Code:

(require 'cl-lib)

(defgroup autovenv nil
  "Automatically activate Python virtual environments."
  :group 'python)

(defcustom autovenv-verbose nil
  "Enable verbose output for debugging."
  :type 'boolean
  :group 'autovenv)

(defcustom autovenv-pre-activate-hook nil
  "Hook run just beore a Python virtualenv is activated"
  :type 'hook
  :group 'autovenv)

(defcustom autovenv-post-activate-hook nil
  "Hook run just after a Python virtualenv is activated"
  :type 'hook
  :group 'autovenv)

(defcustom autovenv-pre-deactivate-hook nil
  "Hook run just before a Python virtualenv is deactivated"
  :type 'hook
  :group 'autovenv)

(defcustom autovenv-postdeactivate-hook nil
  "Hook run just after a Python virtualenv is deactivated"
  :type 'hook
  :group 'autovenv)

(defun autovenv--message (msg)
  (when autovenv-verbose
    (message "autovenv: %s" msg)))

;;
;; Activation/Deactivation (That is, getting/setting the venv)
;;

;;;; Activation/deactivation really consists of
;;;;   - Setting the VIRTUAL_ENV environment variable
;;;;
;;;;   - Updating the PATH environment variable to include the venv's
;;;;     /bin directory
;;;;
;;;;   - Updating exec-path to include venv's /bin directory
;;;;
;;;; Deactivation obviously involves reverting all those changes:
;;;; Removing a directory from PATH and exec-path, and unsetting
;;;; VIRTUAL_ENV.
;;;;
;;;; These functions are solely concerned with manipulating the state
;;;; Python and Emacs need to utilize a venv; any other state autovenv
;;;; maintains to know when to activate/deactivate is not the problem
;;;; of these functions. Similarly, lifecycle management (e.g.,
;;;; calling (deactivate) before calling (activate) when changing
;;;; between venvs) is not in scope

(defun autovenv--activate (venv)
  "Activate the virtual environment at NEW-VENV."
  (autovenv--message (format "Activating %s" venv))
  (run-hooks 'autovenv-pre-activate-hook)
  (let* ((venv-bin (file-name-as-directory
                    (file-name-concat venv "bin"))))
    (setq exec-path (cons venv-bin exec-path))
    (setenv "VIRTUAL_ENV" venv)
    (setenv "PATH" (concat venv-bin path-separator (getenv "PATH"))))
  (run-hooks 'autovenv-post-activate-hook))

(defun autovenv--deactivate ()
  "Deactivate any active virtual environment."
  ;; Grab the venv prior to running pre-deactivate-hook. It's bad form
  ;; to mess with that in the hook, but it's not impossible
  (let* ((venv (getenv "VIRTUAL_ENV"))
         (venv-bin (file-name-as-directory
                    (file-name-concat venv "bin"))))
    (autovenv--message (format "Deactivating %s" venv))
    (run-hooks 'autovenv-pre-deactivate-hook)
    (setq exec-path (delete venv-bin exec-path))
    ;; Split, splice, and rejoin PATH without external dependencies
    (setenv "PATH" (mapconcat
                    'identity
                    (delete venv-bin
                            (split-string (getenv "PATH") path-separator))
                    path-separator))
    (setq exec-path (delete venv-bin exec-path))
    (setenv "VIRTUAL_ENV" nil)
    (run-hooks 'autovenv-post-deactivate-hook)))

(defmacro autovenv--current-venv ()
  "Return the currently configured venv. Syntax sugar over (getenv
\"VIRTUAL_ENV\")"
  '(getenv "VIRTUAL_ENV"))

;;
;; Locating the appropriate virtualenv
;;
;; Locating the virtual environment involves determining the project
;; root, then identifying an associated virtualenv, if one exists.
;;


;;;; Root locating functions

(defun autovenv-local-locator ()
  "Default locator funtion. Looks for a .venv directory in the default
directory of the current buffer. If one is not found, returns the .venv
directory in the nearest parent directory. If neither the current
directory nor any of its parents contain a .venv directory, the function
fails and nil is returned."
  (when-let* ((dir (locate-dominating-file default-directory ".venv")))
    (file-name-as-directory
     (file-name-concat (expand-file-name dir) ".venv"))))

;;;; TODO: Write a global locator that reads project files to identify
;; named virtualenvs and find them in well-known locations, then make
;; the default locator a combinator of the local and global
;; locator. This will require poring through uv, poetry, etc. docs to
;; determine what their behavior is and how to emulate it. The local
;; locator provides good value for now though, and if a user wants to
;; do this work they can.

(defcustom autovenv-virtualenv-locator #'autovenv-local-locator
  "Function used to locate a virtual environment. Takes no arguments and
returns the path to a virtual environment directory, or nil if no
virtual environment can be found.")

(defvar autovenv-virtualenv-path nil
  "If the value of this variable is not nil, it should be a path to a
Python virtual environment. autovenv will use this value
unconditionally, bypassing any other attempts to locate a
virtualenv.

Though this variable can be set globally, it is principally intended for
use as a buffer- or directory-local variable, allowing users to define
arbitrary virtualenvs for use in specific files or projects.")

(defun autovenv--locate-venv ()
  "Find the virtualenv directory, if any, for the current buffer.

If `autovenv-virtualenv-path' is set, it will be used unconditionally. This
variable can be set globally, but is probably best used as a buffer- or
directory-local variable.

If `autovenv-virtualenv-path' is not set, the function defined in
`autovenv-virtualenv-locator' will be used to find a suitable virtualenv for
this buffer. By default, this variable is set to use
`autovenv-default-locator'.

If neither `autovenv-virtualenv-path' nor `autovenv-virtualenv-locator' are set, or if
`autovenv-virtualenv-locator' cannot locate a suitable virtual environment, this
function will return nil, indicating no suitable virtual environment is
present."
  (cond
   ((stringp autovenv-virtualenv-path)
    autovenv-virtualenv-path)
   ((and (functionp autovenv-virtualenv-locator))
    (funcall autovenv-virtualenv-locator))
   (t nil)))





;;
;; Storing and comparing virtualenv state to determine if changes
;; should be made.
;;
;; The state for a given buffer (i.e., the "info") is a list (dir
;; venv), where venv is a path to a virtualenv, and dir is the
;; default-directory associated with the buffer at the time the venv
;; was determined.
;;

(defvar-local autovenv--info nil
  "Contains working info about what the appropriate Python virtualenv
should be for this buffer.")

(defun autovenv--correct-venv ()
  "Return what the \"correct\" venv should be for this buffer, updating
cached information as appropriate."
  (let ((old autovenv--info))
    (setq autovenv--info
          (if (and (not (equal old nil))
                   (equal (car old) default-directory))
              old
            ;; Ensure the output of the locator is formatted as a
            ;; directory (with the trailing slash), since the user may
            ;; be inconsistent
            (let* ((venv-located (autovenv--locate-venv))
                   ;; file-name-as-directory can't handle nils so....
                   (venv-dir (if (eq nil venv-located)
                                 nil
                               (file-name-as-directory venv-located))))
              `(,default-directory ,venv-dir))))
    (cadr autovenv--info)))



;;
;; Entry point -- determine state and activate/deactivate as necessary
;;

(defun autovenv-find-and-activate ()
  (interactive)
  "If a Python virtual environment should be active for the
default-directory of the current buffer, activate it. If not (and one is
activated already), deactivate it.

This function can be used as a hook to automatically activate a virtual
environment when relevant events occur, such as a directory change. To
refresh the virtualenv information when a buffer (more precisely, a
window) is selected, see `autovenv-window-selection-function'."
  ;; Step 1: Find out what the venv should be
  ;; Step 2: Find out what the current venv is
  (let* ((new-venv (autovenv--correct-venv))
         (old-venv (autovenv--current-venv)))
    ;; Step 3: If they're different, change it
    (when (not (eq new-venv old-venv))
      (if (eq new-venv nil)
          (autovenv--deactivate)
        (progn
          ;; Always deactivate the old venv before activating the new
          ;; one, to ensure that things like path variables get
          ;; cleaned up.
          (autovenv--deactivate)
          (autovenv--activate new-venv))))))



(defun autovenv-window-function (window)
  "Function to find and activate an appropriate Python virtual environment
from the `window-buffer-change-functions' and
`window-selection-change-functions' hooks."
  (let* ((buff (window-buffer window)))
    (with-current-buffer buff
      (autovenv-find-and-activate))))

(provide 'autovenv)

;;; autovenv.el ends here
