;; Emacs Configuration.
;; Based on Emacs Prelude.
(defvar ofc-dir (file-name-directory load-file-name) "")
(defvar ofc-vendor-dir (expand-file-name "elisp" ofc-dir) "")
(defvar ofc-modules-dir (expand-file-name  "modules" ofc-dir) "")
(defvar ofc-personal-dir (expand-file-name "personal" ofc-dir) "")
(defvar ofc-savefile-dir (expand-file-name "savefile" ofc-dir) "")
(defvar ofc-snippets-dir (expand-file-name "snippets" ofc-dir) "")
(defvar ofc-custom-file (expand-file-name "custom.el" ofc-savefile-dir) "")

;; Use the old load-path definition, instead of adding all
;; directories under `elisp' to the load-path.
(load (concat ofc-dir "load-path"))

;; Set the theme very early to avoid flashing the unstyled frame.
(load-theme 'warm-night t)

(add-to-list 'load-path ofc-modules-dir)
(add-to-list 'load-path ofc-personal-dir)

;; Prevent customisations from changing this file.  All changes made
;; with Emacs `customize' will be written to the specified.
;; `custom-file'.
(setq custom-file ofc-custom-file)

;; Save Emacs file bookmarks to a different file.
(setq bookmark-default-file (concat ofc-savefile-dir "/" "bookmarks"))

;; Emacs complains if the customisation file is not present on
;; startup. We create create an empty file to avoid this.
(unless (file-exists-p custom-file)
  (write-region "" nil custom-file))

;; Initialize Emacs package management.
(require 'package)
(package-initialize)

;; Load all Mac OS specific Emacs configuration.
(when (eq system-type 'darwin)
  (require 'ofc-osx))

;; Load user-specific settings.
(require 'ofc-personal)

;; Load the main editor components.
(require 'ofc-editor)
(require 'ofc-projectile)
(require 'ofc-yasnippet)
(require 'ofc-recentf)
(require 'ofc-company)
(require 'ofc-coffee)
(require 'ofc-ggtags)
(require 'ofc-magit)
(require 'ofc-helm)
(require 'ofc-web)
(require 'ofc-php)
(require 'ofc-sql)
(require 'ofc-diminish)

;; Load Emacs customisations.
(load custom-file)

(setq bookmark-save-flag t)
(bookmark-bmenu-list)
(switch-to-buffer "*Bookmark List*")
