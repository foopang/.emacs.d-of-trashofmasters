;;; .emacs --- Andrea Turso Emacs configuration file

;; Copyright (c) 2012 Andrea Turso

;; Created: 2012-09-04
;; X-URL: <github url>

(defun ofc/emacs-setup nil
  ;; Set username and email address
  (setq user-full-name "Andrea Turso"
        user-mail-address "trashofmasters@gmail.com")

  (setq inhibit-startup-message t
        initial-scratch-message nil
        truncate-lines t
        overflow-newline-into-fringe t
        tab-width 4
        make-backup-files nil
        backup-inhibited t
        auto-save-default nil
        vc-handled-backends '(Git)
        x-select-enable-clipboard t)

  ;; Smooth scrolling settings
  (setq redisplay-dont-pause t
        scroll-margin 5
        scroll-step 1
        scroll-conservatively 10
        scroll-preserve-screen-position 1)

  ;; Answer just y/n insted of yes or no
  (fset 'yes-or-no-p 'y-or-n-p)

  (put 'narrow-to-region 'disabled nil)
  (put 'narrow-to-page 'disabled nil)

  (auto-save-mode -1)
  (tooltip-mode -1)
  (blink-cursor-mode -1)
  ;;(display-battery-mode t)
  (show-paren-mode t)
  (column-number-mode t)
  ;;(sml-modeline-mode t) Doesn't work in emacs 24.2

  (setq-default indent-tabs-mode nil)

  ;; create the autosave dir if necessary
  ;;(make-directory "~/.emacs.autosaves/" t)
  )

(defun* get-closest-pathname (&optional (file "Amkfile"))
  "Determine the pathname of the closes Amkfile to be passed to Amaka when it's run from Emacs."
  (let ((root (expand-file-name "/"))) ; the win32 builds should translate this correctly
    (expand-file-name file
		      (loop
                       for d = default-directory then (expand-file-name ".." d)
                       if (file-exists-p (expand-file-name file d))
                       return d
                       if (equal d root)
                       return nil))))

(defun ofc/normalise-file-hook nil
  "Normalize a file"
  (delete-trailing-whitespace))

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
        ido-work-directory-list '("~/Development")
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
        ido-max-work-file-list 3      ; remember many
        ido-use-filename-at-point nil       ; don't use filename at point (annoying)
        ido-use-url-at-point nil            ; don't use url at point (annoying)
        ido-enable-flex-matching nil        ; don't try to be too smart
        ido-max-prospects 10                ; don't spam my minibuffer
        ido-confirm-unique-completion t
        ido-file-extensions-order '(".org"   ".php" ".phtml"
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
          erc-autojoin-channels-alist '((".*\\.freenode.net" "#zftalk" "#composer" "#php")
                                        (".*\\.azzurra.org" "#azzurragamers")))

    (erc :server "irc.freenode.net" :port 6667 :nick "trashofmasters" :password "" :full-name "Andrea")
    (erc :server "irc.azzurra.org" :port 6667 :nick "trashofmasters" :full-name "Andrea")))

;;
(defun ofc/ido-minibuffer-hook nil
  "Ido minibuffer hook"
  (defvar resize-minibuffer-window-max-height)

  (make-local-variable 'resize-minibuffer-window-max-height)
  (setq resize-minibuffer-window-max-height 1))

(defun ofc/php-lint ()
  (interactive)
  (compile (format "/usr/local/bin/amaka -f %s test" (get-closest-pathname))))

;; PHP mode hook
(defun ofc/php-mode-hook nil
  "Php-mode hook"
  ;;
  (flymake-php-load)
  (eldoc-mode)
  (local-set-key [f8] 'ofc/php-lint))

 ;;
(defun ofc/keybindings-install nil
  (global-unset-key (kbd "C-z"))
  (global-unset-key (kbd "C-x C-b"))
  (global-unset-key (kbd "C-x C-l"))
  (global-set-key (kbd "M-g") 'goto-line)
  (global-set-key (kbd "M-g") 'goto-line)
  ;; (global-set-key (kbd "RET") 'newline-and-indent)
  (global-set-key "\C-x\C-c" 'kill-some-buffers))


;; Org-mode install
(defun ofc/org-install nil
  "Org mode customisations"

  ;; keyboard binds
  (global-set-key (kbd "\C-c l") 'org-cycle-agenda-files)
  (global-set-key (kbd "\C-c a") 'org-agenda)
  (global-set-key (kbd "\C-c b") 'org-iswitchb) ; Switch between multiple org files

  ;;(org-clock-persistence-insinuate)

  ;; Embed stylesheet in the html file rather than linking it.
  (setq org-export-html-style
        (concat "<style type=\"text/css\">"
                (with-temp-buffer
                  (insert-file-contents "~/.org.css")
                  (buffer-string))
                "</style>"))

  ;; Link the stylesheet in the html file.
  ;; (setq org-export-html-style "<link rel=\"stylesheet\" type=\"text/css\" href=\"mystyles.css\">")
  (setq org-log-done 'time
        org-default-notes-file "~/Personal/scratch.org"
        org-agenda-files "~/organizers"
        org-todo-keyword-faces '(("PROGRESS" . (:foreground "yellow" :weight bold)))
        org-todo-keyword-faces '(("CANCELED" . (:foreground "gray" :weight bold)))
        org-todo-keywords (quote ((sequence "TODO(t)" "NEXT(n)" "STARTED(s)" "|" "DONE(d!/!)")
                                  (sequence "WAITING(w@/!)" "SOMEDAY(S!)" "|" "CANCELLED(c@/!)" "PHONE")
                                  (sequence "OPEN(O!)" "|" "CLOSED(C!)")))))

;; Configure autoloading
(setq load-path
      (append (list "~/.emacs.d"
                    "~/.emacs.d/modes/php-mode"
                    "~/.emacs.d/plugins/auto-complete"
                    "~/.emacs.d/plugins/php-extras")
              load-path))

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")

;; autoload php mode only when php files are opened
(autoload 'php-mode "php-mode" "Major mode for editing php code." t)
(autoload 'feature-mode "feature-mode" "Mode that supports Cucumber syntax." t)

;; add file associations
(push '("\\.phtml" . html-mode) auto-mode-alist)
(push '("\\.feature" . feature-mode) auto-mode-alist)
(push '("\\.markdown" . markdown-mode) auto-mode-alist)
(push '("\\.php\\|Amkfile" . php-mode) auto-mode-alist)
(push '("\\.Rakefile\\|.rb" . ruby-mode) auto-mode-alist)

;; Customize PHP buffers settings
(add-hook 'php-mode-hook 'ofc/php-mode-hook)

;; Remove trailing whitespaces when saving files
(add-hook 'before-save-hook 'ofc/normalise-file-hook)
(add-hook 'after-save-hook 'ofc/dot-emacs-autocompile)

;; Customize ido minibuffer
(add-hook 'ido-minibuffer-setup-hook 'ofc/ido-minibuffer-hook)

;; Emacs specific setup
(ofc/emacs-setup)

;; Mode specific loading and setup
(ofc/org-install)
(ofc/ido-install)
(ofc/keybindings-install)

(require 'package)
(require 'php-extras)
(require 'auto-complete-config)

(load-theme 'ir-black t)

(ac-config-default)
(setq ac-dwim t)
(setq ac-auto-start 3)
(setq ac-auto-show-menu 0.5)

(add-to-list 'package-archives
    '("marmalade" .
      "http://marmalade-repo.org/packages/"))

(package-initialize)

(eval-after-load 'auto-complete-config '(add-to-list 'ac-dictionary-directories "~/.emacs.d/plugins/auto-complete/ac-dict"))
(eval-after-load 'sql '(lambda () (sql-set-product 'mysql)))
