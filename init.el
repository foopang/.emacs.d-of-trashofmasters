;;; .emacs --- Andrea Turso Emacs configuration file

;;; Created: 2012-09-04
;;;
;;; Changelog
;;;
;;; 2014-09-11
;;; - Reorganised various setq blocks into the ofc/emacs-setup procedure.
;;;
;;; 2014-09-14
;;; - Broken down this file into bindings.el, defuns.el and hooks.el
;;;
;;; 2014-09-15
;;; - Added `saveplace' which should help keep track of workflow.
;;; - Added new themes and moved the load-theme form up top.
;;; - Increased the size of the default frame created on startup,
;;;   however this isn't affecting Magit buffers being created in their own frame
;;;   See http://whattheemacsd.com/setup-magit.el-01.html on how to avoid using a
;;;   different frame altogether. Remember that `defadvice' is a
;;;   cleaner method for a library to customize functions defined
;;;   within Emacs.
;;;
;;; 2014-09-17
;;; - Removed `initial-frame-alist' and  `default-frame-alist' as they were causing
;;;   text rendering issues in the buffer until the window was resized.
;;; - Set both Alt and Command key to behave as meta.
;;;
;;; 2014-09-22
;;; - Customise configuration are now saved to `~/.emacs-custom.el'

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

(when (fboundp 'blink-cursor-mode)
  (blink-cursor-mode -1))

;; Tell Emacs custom to save to a separate file.
(setq custom-file "~/.emacs-custom.el")

;; Create custom file if it doesn't exist, to avoid init errors.
(unless (file-exists-p custom-file)
 (write-region "" nil custom-file))

;; Load the various modules used in this configuration.
(load "~/.emacs.d/bindings")
(load "~/.emacs.d/hooks")
(load "~/.emacs.d/defuns")

;; Load emacs customisations
(load custom-file)

;; Initiate emacs configuration.
(ofc/setup)

;; Loading emacs-color-themes works only in this form.
(load-theme 'odersky t)

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

;; Highlight mysql keywords
(add-hook 'sql-mode 'ofc/sql-mode-hook)

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
(require 'yasnippet)
(require 'flx-ido)
(require 'highlight-symbol)

;; Initialise projectile project management mode.
(projectile-global-mode)

;; Make dired less verbose
;; (require 'dired-details)
;; (setq-default dired-details-hidden-string "- ")
;; (dired-details-install)

;; Mode specific loading and setup
(ofc/ido)
(ofc/keybindings)

;; Reload the snippets (once) after yasnippet is loaded.
(eval-after-load 'yasnippet (lambda () (yas-reload-all)))
