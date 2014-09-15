;;; .emacs --- Andrea Turso Emacs configuration file

;;; Created: 2012-09-04
;;;
;;; Changelog
;;;
;;; - 2014-09-11
;;;   Reorganised various setq blocks into the ofc/emacs-setup procedure.
;;;
;;; - 2014-09-14
;;;   Broken down this file into bindings.el, defuns.el and hooks.el

(setq initial-frame-alist '((width . 180) (height . 60)))
(setq default-frame-alist '((width . 180) (height . 60)))

(when (fboundp 'fringe-mode)
  (fringe-mode -1))

(when (fboundp 'tooltip-mode)
  (tooltip-mode -1))

(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))

(when (fboundp 'menu-bar-mode)
  (menu-bar-mode -1))

(when (fboundp 'auto-save-mode)
  (auto-save-mode -1))

(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

(when (fboundp 'bling-cursor-mode)
  (blink-cursor-mode -1))

;; Load the various modules used in this configuration.
(load "~/.emacs.d/hooks")
(load "~/.emacs.d/bindings")
(load "~/.emacs.d/defuns")

;; Initiate emacs configuration.
(ofc/setup)

;; Loading emacs-color-themes works only in this form.
(load-theme 'wilson t)

;; Autoload php mode only when php files are opened.
(autoload 'php-mode "php-mode" "Major mode for editing php code." t)
(autoload 'feature-mode "feature-mode" "Mode that supports Cucumber syntax." t)

;; add file associations
(push '("\\.p?html" . web-mode) auto-mode-alist)
(push '("\\.twig" . web-mode) auto-mode-alist)
(push '("\\.feature" . feature-mode) auto-mode-alist)
(push '("\\.markdown" . markdown-mode) auto-mode-alist)
(push '("\\.php" . php-mode) auto-mode-alist)
(push '("\\.Rakefile\\|.rb" . ruby-mode) auto-mode-alist)
(push '("\\.coffee" . coffee-mode) auto-mode-alist)

;; Remove trailing whitespaces when saving files
(add-hook 'write-file-hooks 'delete-trailing-whitespace)
;; Bytecompile the emacs init file to speed-up loading
(add-hook 'after-save-hook 'ofc/dot-emacs-autocompile)
;; Configure the look&feel of the ido minibuffer
(add-hook 'ido-minibuffer-setup-hook 'ofc/ido-minibuffer-hook)

;; Configure php-mode
(add-hook 'php-mode-hook 'ofc/php-mode-hook)

;; Emacs specific setup
(ofc/custom)

;; Initialise Emacs package management.
(package-initialize)

;; I think I should autoload the packages below as well.
(require 'package)
(require 'saveplace)
(require 'web-mode)
(require 'coffee-mode)
(require 'sr-speedbar)
(require 'php-extras)

;; Make dired less verbose
;; (require 'dired-details)
;; (setq-default dired-details-hidden-string "- ")
;; (dired-details-install)

;; Mode specific loading and setup
(ofc/ido)
(ofc/keybindings)

;; Highlight mysql keywords
(eval-after-load 'sql '(lambda () (sql-set-product 'mysql)))

;; Save point position between sessions
(setq-default save-place t)
(setq save-place-file (expand-file-name "places" user-emacs-directory))
