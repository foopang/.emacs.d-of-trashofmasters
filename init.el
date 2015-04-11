;;; init.el --- Personal Emacs Configuration File

;; Copyright (C) 2014 Andrea Turso

;; Author: Andrea Turso <trashofmasters@gmail.com>
;; URL: https://github.com/trashofmasters/.emacs.d

;; This file is not part of GNU Emacs
;;
;; You'll notice that some of the structure of this
;; project is clumsily copied from the great Emacs
;; Prelude package.

(defvar ofc-dir (file-name-directory load-file-name)
  "The Emacs configuration directory, generally this is `~/.emacs/d'.")

(defvar ofc-elisp-dir (expand-file-name "elisp" ofc-dir)
  "The directory where third-party packages should be installed.
At this time packages are manually checked-out or downloaded and
added to this directory. When using this configuration on a new
machine remember to install all packages required in `load-path.el'.")

(defvar ofc-modules-dir (expand-file-name  "modules" ofc-dir)
  "The directory where all modules that make up this Emacs configuration
are stored. All files in this directory will be available to load once
the directory is added to the `load-path' variable.")

(defvar ofc-personal-dir (expand-file-name "personal" ofc-dir)
  "The directory containing all personal configuration overrides.")

(defvar ofc-savefile-dir (expand-file-name "savefile" ofc-dir)
  "The directory where all persistent Emacs configuration will be stored.")

(defvar ofc-snippets-dir (expand-file-name "snippets" ofc-dir)
  "The directory where Yasnippet will look for additional snippets to load.")

(defvar ofc-custom-file (expand-file-name "custom.el" ofc-savefile-dir)
  "The name of the configuration file where `customize' will save
our changed Emacs paramaters.")

;; Use the old load-path definition, instead of adding all
;; directories under `elisp' to the load-path.
(load (expand-file-name "load-path" ofc-dir))

;; Set the following appearance options early to avoid
;; flashing an unstyled frame, menu or scroll bars.
(fringe-mode -1)
(tooltip-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(auto-save-mode -1)
(scroll-bar-mode -1)
(blink-cursor-mode -1)
;;(load-theme 'warm-night t)

(add-to-list 'load-path ofc-modules-dir)
(add-to-list 'load-path ofc-personal-dir)

;; Prevent customisations from changing this file.  All changes made
;; with Emacs `customize' will be written to the specified.
;; `custom-file'.
(setq custom-file ofc-custom-file)

;; Save Emacs file bookmarks to a different file.
(setq bookmark-default-file (expand-file-name "bookmarks" ofc-savefile-dir))

;; Emacs complains if the customisation file is not present on
;; startup. We create create an empty file to avoid this.
(unless (file-exists-p custom-file)
  (write-region "" nil custom-file))

;; Initialize Emacs package management.
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)
(load-theme 'solarized-dark t)

;; Load all Mac OS specific Emacs configuration
;; when running Emacs on a Mac.
(when (eq system-type 'darwin)
  (require 'ofc-osx))

;; Load user-specific settings.
(require 'ofc-personal)

;; Diminish all vanilla emacs minor modes.  The rest of the
;; minor-modes can be diminished individually in their own
;; configuration files.
(require 'ofc-diminish)

;; Load the main editor components and configuration.
(require 'ofc-editor)

;; Load and configure the Emacs Code Browser component.
;;
;; TODO Move the important bits and bobs of its configuration
;; from the custom file into ofc-ecb.el.
;;(require 'ofc-projectile)
(require 'ofc-helm)
(require 'ofc-ecb)

(require 'ofc-yasnippet)
(require 'ofc-recentf)
(require 'ofc-company)
(require 'ofc-magit)

(require 'ofc-tags)

(require 'ofc-coffee)
(require 'ofc-sql)
(require 'ofc-php)
(require 'ofc-web)
(require 'ofc-org)

;; Load Emacs customisations.
(load custom-file)
