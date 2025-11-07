;;; pg-autovenv.el --- Automatically activate Python virtualenvs based on project directory -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Phil Groce

;; Author: pgroce <pgroce@gmail.com>
;; Version: 0.1.0
;; Keywords: python, virtualenv, environment, tools, projects
;; Package-Requires: ((cl-lib "0.5"))
;; License: GPL-3.0-or-later

;;; Commentary:
;;
;; This is a fork of auto-virtualenv.el
;; <https://github.com/marcwebbie/auto-virtualenv> by Markwebbie. The
;; package name has been changed, and some of the code simplified for
;; my use. The original commentary (with the package name updated)
;; follows:
;;
;; Auto Virtualenv is a powerful Emacs package for Python developers, offering
;; automatic virtual environment management based on the directory of the current
;; project. This tool simplifies working across multiple Python projects by
;; dynamically detecting and activating virtual environments, reducing the need
;; for manual configuration.
;;
;; It integrates seamlessly with `lsp-mode` and `pyright`, optionally reloading
;; the LSP workspace upon environment activation to maintain accurate imports and
;; environment settings. Auto Virtualenv identifies Python projects using a
;; customizable set of markers (e.g., `setup.py`, `pyproject.toml`) and supports
;; common virtual environment locations, both local and global (e.g., `~/.pyenv/versions/`).
;;
;; Features:
;; - **Automatic Virtual Environment Detection and Activation**: Based on project root,
;;   pg-autovenv locates and activates virtual environments in either a local
;;   project directory or in specified global directories.
;; - **LSP Reload Support**: With `lsp-mode` or `pyright`, optionally reload the LSP workspace
;;   on environment changes to keep code assistance up-to-date.
;; - **Modeline Integration**: Displays the active environment in the modeline. When no
;;   environment is active, "Venv: N/A" is shown.
;; - **Configurable and Extensible**: Users can add directories for environment searches, set
;;   custom project markers, and control verbosity for debugging.
;;
;; Usage:
;; 1. Add `pg-autovenv` to your `load-path` and enable it with `pg-autovenv-setup`.
;; 2. Configure `pg-autovenv-global-dirs`, `pg-autovenv-python-project-files`,
;;    and `pg-autovenv-reload-lsp` as needed.
;; 3. Use it with project management packages like `projectile` or independently.
;;
;; See the README for detailed setup and configuration examples.
;;
;; (End of original commentary)
;;
;; Recommended usage of this package is via the use-package macro, for instance:
;;
;;   (use-package pg-autovenv
;;     :ensure t
;;     :custom
;;     (pg-autovenv-verbose t)
;;     :hook ((find-file-hook
;;             eshell-directory-change-hook) . pg-autovenv-find-and-activate))
;;
;; Modes can be changed to suit your application.
;;
;;; Code:

(require 'cl-lib)

(defgroup pg-autovenv nil
  "Automatically activate Python virtual environments."
  :group 'python)

(defcustom pg-autovenv-global-dirs
  '("~/.virtualenvs/" "~/.pyenv/versions/" "~/.envs/" "~/.conda/" "~/.conda/envs/")
  "List of global directories to search for virtual environments by project name."
  :type '(repeat string)
  :group 'pg-autovenv)

(defcustom pg-autovenv-local-dirs
  '(".venv" "venv")
  "List of local directories to search for virtual environments."
  :type '(repeat string)
  :group 'pg-autovenv)

(defcustom pg-autovenv-python-project-files
  '("requirements.txt" "Pipfile" "pyproject.toml" "setup.py" "manage.py" "tox.ini"
    ".flake8" "pytest.ini" ".pre-commit-config.yaml" "environment.yml"
    "__init__.py" "*.py" ".python-version")
  "List of files that identify a Python project."
  :type '(repeat string)
  :group 'pg-autovenv)

(defcustom pg-autovenv-reload-lsp t
  "Automatically reload `lsp-mode` or `pyright` when changing virtual environments."
  :type 'boolean
  :group 'pg-autovenv)

(defcustom pg-autovenv-verbose t
  "Enable verbose output for debugging."
  :type 'boolean
  :group 'pg-autovenv)

(defcustom pg-autoenv-mode-line-prefix "Venv:"
  "Prefix to venv name in mode line.")

(provide 'pg-autovenv)

;;; pg-autovenv.el ends here
