;; Emacs Configuration
;; Based on Emacs Prelude.
(defvar ofc-dir (file-name-directory load-file-name) "")
(defvar ofc-vendor-dir (expand-file-name "elisp" ofc-dir) "")
(defvar ofc-modules-dir (expand-file-name  "modules" ofc-dir) "")
(defvar ofc-personal-dir (expand-file-name "personal" ofc-dir) "")
(defvar ofc-savefile-dir (expand-file-name "savefile" ofc-dir) "")
(defvar ofc-snippets-dir (expand-file-name "snippets" ofc-dir) "")
(defvar ofc-custom-file (expand-file-name "custom.el" ofc-dir) "")

;; Use the old load-path definition, instead of adding all
;; directories under `elisp' to the load-path.
(load (concat ofc-dir "load-path"))

(add-to-list 'load-path ofc-modules-dir)
(add-to-list 'load-path ofc-personal-dir)

;; Prevent customisations from changing this file.  All changes made
;; with Emacs `customize' will be written to the specified.
;; `custom-file'.
(setq custom-file ofc-custom-file)

;; Save Emacs file bookmarks to a different file.
(setq bookmark-default-file (concat ofc-savefile-dir "bookmarks"))

;; Emacs complains if the customisation file is not present on
;; startup. We create create an empty file to avoid this.
(unless (file-exists-p custom-file)
  (write-region "" nil custom-file))

;; Initialize Emacs package management.
(require 'package)
(package-initialize)

(require 'moe-theme)
(moe-dark)
(moe-theme-set-color 'blue)

;; Load all Mac OS specific Emacs configuration.
(when (eq system-type 'darwin)
  (require 'ofc-osx))

;; Load user-specific settings.
(require 'ofc-personal)

;; Load the main editor components.
(require 'ofc-projectile)
(require 'ofc-recentf)
(require 'ofc-company)
(require 'ofc-editor)
(require 'ofc-coffee)
(require 'ofc-magit)
(require 'ofc-helm)
(require 'ofc-web)
(require 'ofc-php)
(require 'ofc-sql)

;; Load Emacs customisations.
(load custom-file)
