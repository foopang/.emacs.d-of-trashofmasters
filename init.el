;;; .emacs --- Andrea Turso Emacs configuration file

;;; Created: 2012-09-04
;;;
;;; Changelog
;;;
;;; - 2014-09-11
;;;   Reorganised various setq blocks into the ofc/emacs-setup procedure.

(defun ofc/emacs-setup nil
  ;; Configure autoloading
  ;; the code below needs each individual directory to be added
  (setq load-path (append (list "~/.emacs.d/elisp/php-mode"
                                "~/.emacs.d/elisp/php-extras"
                                "~/.emacs.d/elisp/php-mode"
                                "~/.emacs.d/elisp/popup-el"
                                "~/.emacs.d/elisp/coffee-mode"
                                "~/.emacs.d/elisp/web-mode"
                                "~/.emacs.d/elisp/cl-lib"
                                "~/.emacs.d/elisp/sr-speedbar"
                                "~/.emacs.d/elisp/auto-complete")
                          load-path))

  ;;
  (setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                           ("marmalade" . "http://marmalade-repo.org/packages/")
                           ("melpa" . "http://melpa.milkbox.net/packages/"))))

;; Custom global key bindings
(defun ofc/keybindings-install nil
  (global-unset-key (kbd "C-z"))

  ;; Prevent C-x C-b from quitting emacs.
  (global-unset-key (kbd "C-x C-b"))
  (global-unset-key (kbd "C-x C-l"))

  (global-set-key (kbd "M-g") 'goto-line)
  (global-set-key (kbd "M-§") 'sr-speedbar-toggle)
  (global-set-key (kbd "C-c C-a") 'mark-whole-buffer)
  (global-set-key (kbd "C-x a r") 'align-regexp)
  (global-set-key (kbd "C-x C-c") 'kill-some-buffers))

;; Org-mode install
(defun ofc/org-install nil
  "Org mode customisations"
  ;; keyboard binds
  (global-set-key (kbd "\C-c l") 'org-cycle-agenda-files)
  (global-set-key (kbd "\C-c a") 'org-agenda)
  (global-set-key (kbd "\C-c b") 'org-iswitchb) ; Switch between multiple org files

  ;; Link the stylesheet in the html file.
  (setq org-log-done 'time
        org-todo-keyword-faces '(("PROGRESS" . (:foreground "yellow" :weight bold)))
        org-todo-keyword-faces '(("CANCELED" . (:foreground "gray" :weight bold)))
        org-todo-keywords (quote ((sequence "TODO(t)" "NEXT(n)" "STARTED(s)" "|" "DONE(d!/!)")
                                  (sequence "WAITING(w@/!)" "SOMEDAY(S!)" "|" "CANCELLED(c@/!)" "PHONE")
                                  (sequence "OPEN(O!)" "|" "CLOSED(C!)")))))

(defun ofc/andrea-config nil
  ;; Set username and email address
  (setq user-full-name "Andrea Turso"
        user-mail-address "trashofmasters@gmail.com")

  (setq special-display-buffer-names
        (nconc '("*Backtrace*" "*VC-log*" "*compilation*" "*grep*")
               special-display-buffer-names))

  (fringe-mode -1)
  (tooltip-mode -1)
  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (auto-save-mode -1)
  (scroll-bar-mode -1)
  (blink-cursor-mode -1)

  (show-paren-mode t)
  (column-number-mode t)
  (display-battery-mode t)

  (setq-default indent-tabs-mode nil)

  (setq coffee-tab-width 2
        tab-width 4)

  ;; Add newline to the end of a file.
  ;; change 't to 'query to ask when necessary.
  (setq require-final-newline 't)

  ;;
  (setq inhibit-startup-message t
        initial-scratch-message nil
        truncate-lines t
        backup-inhibited t
        overflow-newline-into-fringe t
        vc-handled-backends '(Git))

  ;; Disable backup files
  (setq make-backup-files nil
        auto-save-default nil)

  ;; Use the X11 clipboard
  (setq x-select-enable-clipboard t)

  ;; Speedbar configuration
  (setq speedbar-show-unknown-files t
        speedbar-verbosity-level 0
        ;;speedbar-show-unknown-files nil
        ;;speedbar-smart-directory-expand-flag t
        ;;speedbar-hide-button-brackets-flag nil

        sr-speedbar-right-side nil
        sr-speedbar-refresh-turn-off t
        sr-speedbar-skip-other-window-p t)

  ;; Smooth scrolling settings
  (setq scroll-step 1
        scroll-margin 5
        redisplay-dont-pause t
        scroll-conservatively 10
        scroll-preserve-screen-position 1)

  ;; Answer just y/n insted of yes or no
  (fset 'yes-or-no-p 'y-or-n-p)

  ;; Don't ask before using narrow commands.
  (put 'narrow-to-region 'disabled nil)
  (put 'narrow-to-page 'disabled nil))

;; Bytcompile the .emacs configuration file when it gets saved
(defun ofc/dot-emacs-autocompile nil
  "Bytecompile the file if is ~/.emacs"
  (interactive)
  (require 'bytecomp)
  (if (string= (buffer-file-name) (expand-file-name (concat default-directory ".emacs")))
      (byte-compile-file (buffer-file-name))))

;; Installl Lusty explorer for opening files
(defun ofc/lusty-explorer-install nil
  "Overrride the normal file-opening and buffer switching with the lusty explorer"
  (when (require 'lusty-explorer nil 'noerror)
    (global-set-key (kbd "C-x C-f") 'lusty-file-explorer)
    (global-set-key (kbd "C-x b")   'lusty-buffer-explorer)))

;; Install iDO mode
(defun ofc/ido-install nil
  (defun ido-disable-line-trucation () (set (make-local-variable 'truncate-lines) nil))

  "Override normal file-opening and buffer switching with iDO"
  (ido-mode t)
  (add-hook 'ido-minibuffer-setup-hook 'ido-disable-line-trucation)
  (setq confirm-nonexistent-file-or-buffer nil
        ido-create-new-buffer 'always
        ido-work-directory-list '("~/JustPark")
        ido-decorations '("\n ➥ "
                          ""
                          "\n   "
                          "\n..."
                          "[" "]"
                          " [No match]"
                          " [Matched]" " [Not readable]"
                          " [Too big]" " [Confirm]")
        ido-enable-last-directory-history t ; remember last used dirs
        ido-max-work-directory-list 25      ; should be enough
        ido-max-work-file-list 3            ; remember many
        ido-use-filename-at-point nil       ; don't use filename at point (annoying)
        ido-use-url-at-point nil            ; don't use url at point (annoying)
        ido-enable-flex-matching nil        ; don't try to be too smart
        ido-max-prospects 10                ; don't spam my minibuffer
        ido-auto-merge-delay-time 10        ; Time (in seconds) before ido-find-file will attempt to guess the file location



        ido-confirm-unique-completion nil
        ido-file-extensions-order '(".php" ".html"
                                    ".twig" ".js" ".coffee"
                                    ".html"  ".xml" ".sql"
                                    ".emacs" ".ini" ".cfg")))

;; ERC the IRC Client startup function
(defun ofc/erc-startup nil
  "Startup ERC the IRC Client."
  (interactive)

  (defvar erc-hide-list)
  (defvar erc-track-exclude-types)
  (defvar erc-autojoin-channels-alist)

  (when (y-or-n-p "Would you like to start ERC the IRC Client now? ")
    (erc-autojoin-mode t)

    (setq erc-hide-list '("JOIN" "PART" "QUIT" "NICK")
          erc-autojoin-channels-alist '((".*\\.freenode.net" "#laravel-offtopic" "#laravel")))

    (erc :server "irc.freenode.net" :port 6667 :nick "trashofmasters" :password "" :full-name "")))

;;
(defun ofc/ido-minibuffer-hook nil
  "Ido minibuffer hook"
  (defvar resize-minibuffer-window-max-height)

  (make-local-variable 'resize-minibuffer-window-max-height)
  (setq resize-minibuffer-window-max-height 1))


(add-to-list 'custom-theme-load-path "~/.emacs.d/elisp/themes")

;; Autoload php mode only when php files are opened
(autoload 'php-mode "php-mode" "Major mode for editing php code." t)
(autoload 'feature-mode "feature-mode" "Mode that supports Cucumber syntax." t)

;; add file associations
(push '("\\.phtml" . html-mode) auto-mode-alist)
(push '("\\.twig" . html-mode) auto-mode-alist)
(push '("\\.feature" . feature-mode) auto-mode-alist)
(push '("\\.markdown" . markdown-mode) auto-mode-alist)
(push '("\\.php" . php-mode) auto-mode-alist)
(push '("\\.Rakefile\\|.rb" . ruby-mode) auto-mode-alist)
(push '("\\.coffee" . coffee-mode) auto-mode-alist)

;; Remove trailing whitespaces when saving files
(add-hook 'write-file-hooks 'delete-trailing-whitespace)
(add-hook 'after-save-hook 'ofc/dot-emacs-autocompile)

;; CUSTOMIZE ido minibuffer
(add-hook 'ido-minibuffer-setup-hook 'ofc/ido-minibuffer-hook)

;; Emacs specific setup
(ofc/emacs-setup)
(ofc/andrea-config)

(require 'web-mode)
(require 'coffee-mode)
(require 'sr-speedbar)
(require 'cl-lib)
(require 'auto-complete-config)
(require 'package)

(package-initialize)

;; Mode specific loading and setup
(ofc/org-install)
(ofc/ido-install)
(ofc/keybindings-install)

;;(load-theme 'zenburn)
(load-theme 'wombat)

(ac-config-default)
(setq ac-dwim t)
(setq ac-auto-start 4)
(setq ac-auto-show-menu 0.5)
(setq ac-menu-height 10)
;; These colours go well with wombat theme.
(set-face-underline 'ac-candidate-face "Gray13")
(set-face-background 'ac-candidate-face "Gray13")
(set-face-foreground 'ac-candidate-face "YellowGreen")
(set-face-background 'ac-selection-face "PaleGreen")
(set-face-foreground 'ac-selection-face "Gray13")

(defun comment-or-uncomment-line-or-region ()
  "Comments or uncomments the current line or region."
  (interactive)
  (if (region-active-p)
      (comment-or-uncomment-region (region-beginning) (region-end))
    (comment-or-uncomment-region (line-beginning-position) (line-end-position))))

(global-set-key (kbd "C-/") 'comment-or-uncomment-line-or-region)

;;(eval-after-load 'sql '(lambda () (sql-set-product 'mysql)))
;;(eval-after-load 'php-mode (require 'php-extras))

(when (and (fboundp 'toggle-frame-maximized) window-system) (toggle-frame-maximized))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#242424" "#e5786d" "#95e454" "#cae682" "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"])
 '(custom-safe-themes
   (quote
    ("3c9d994e18db86ae397d077b6324bfdc445ecc7dc81bb9d528cd9bba08c1dac1" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
