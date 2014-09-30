(defun comment-or-uncomment-line-or-region ()
  "Comments or uncomments the current line or region."
  (interactive)
  (if (region-active-p)
      (comment-or-uncomment-region (region-beginning) (region-end))
    (comment-or-uncomment-region (line-beginning-position) (line-end-position))))

;;
(defun ofc/prompt-before-closing ()
  "Prevent Emacs from suddenly closing during a keystroke frenzy unless 'y' was pressed"
  (interactive)
  (if (y-or-n-p (format "Are you sure you want to exit Emacs? "))
      (if (< emacs-major-version 22)
          (save-buffers-kill-terminal)
        (save-buffers-kill-emacs))
    (message "No exit.")))

(defun ofc/setup nil
  "Configure emacs autoload paths"

  (add-to-list 'custom-theme-load-path "~/.emacs.d/elisp/themes")

  (setq load-path (append (list "~/.emacs.d/elisp/php-mode"
                                "~/.emacs.d/elisp/php-eldoc"
                                "~/.emacs.d/elisp/php-extras"
                                "~/.emacs.d/elisp/helm-company"
                                "~/.emacs.d/elisp/coffee-mode"
                                "~/.emacs.d/elisp/web-mode"
                                "~/.emacs.d/elisp/yasnippet"
                                "~/.emacs.d/elisp/helm"
                                "~/.emacs.d/elisp/highlight-symbol"
                                "~/.emacs.d/elisp/sr-speedbar")
                          load-path))

  ;;
  (setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                           ("marmalade" . "http://marmalade-repo.org/packages/")
                           ("melpa" . "http://melpa.milkbox.net/packages/"))))

(defun ofc/custom nil
  "Andrea's customisations"

  ;; Set username and email address
  (setq user-full-name "Andrea Turso"
        user-mail-address "trashofmasters@gmail.com")

  ;; special-display-regexps has been obsoleted in Emacs 24.3
  ;; In the meanwhile I rely on it to display Magit buffers
  ;; in their own frame. However this doesn't seem to play well
  ;; with the native OSX fullscreen.
  (setq special-display-regexps '("\\*magit:.*\\*"))

  (setq special-display-buffer-names
        (nconc '("*Backtrace*" "*VC-log*" "*compilation*" "*grep*")
               special-display-buffer-names))

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
        overflow-newline-into-fringe t)

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
  (put 'narrow-to-page 'disabled nil)
  (put 'dired-find-alternate-file 'disabled nil)

  ;; Save point position between sessions
  (setq-default save-place t)
  (setq save-place-file (expand-file-name "places" user-emacs-directory)))


;; Bytcompile the .emacs configuration file when it gets saved
(defun ofc/dot-emacs-autocompile nil
  "Bytecompile the file if is ~/.emacs"
  (interactive)
  (require 'bytecomp)
  (if (string= (buffer-file-name) (expand-file-name (concat default-directory ".emacs")))
      (byte-compile-file (buffer-file-name))))

(defun ofc/goto-line-with-feedback nil
  "Show line numbers temporarily, while prompting for the line number input"
  (interactive)
  (unwind-protect
      (progn
        (linum-mode 1)
        (goto-line (read-number "Goto line: ")))
    (linum-mode -1)))

(defun ofc/ido nil
  "Override normal file-opening and buffer switching with ido"
  (defun ido-disable-line-trucation () (set (make-local-variable 'truncate-lines) nil))

  (ido-mode 1)
  (ido-everywhere 1)
  (flx-ido-mode 1)

  ;; disable ido faces to see flx highlights.
  (setq ido-enable-flex-matching t)
  (setq ido-use-faces nil)

  (add-hook 'ido-minibuffer-setup-hook 'ido-disable-line-trucation)
  (setq confirm-nonexistent-file-or-buffer nil
        ido-create-new-buffer 'always
        ido-work-directory-list '("~/Dev"
                                  "~/JustPark/api"
                                  "~/JustPark/admin")
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
        ido-confirm-unique-completion nil   ; stop automatic selection of sole completions.
        ido-file-extensions-order '(".php" ".html"
                                    ".twig" ".js" ".coffee"
                                    ".html"  ".xml" ".sql"
                                    ".emacs" ".ini" ".cfg")))

(defun ofc/yasnippet nil
  "Install yasnippet"
  (setq yas-snippet-dirs '("~/.emacs.d/snippets"))
  (yas-global-mode 1))

(defun ofc/helm nil
  "Customise Helm"
  (setq helm-display-function
      (lambda (buf)
        (split-window-vertically)
        (other-window 1)
        (switch-to-buffer buf)))
  (helm-mode 1))

(defun ofc/company nil
  "Customise Company mode"

  ;; Set the list of company backends that I use.
  (add-to-list 'company-backends 'company-dabbrev-code)

  ;; Bind C-; to autocomplete with company in a helm buffer.
  ;; @note: What is the (non-obvious) difference between define-key and global-set-key?
  (define-key company-mode-map (kbd "C-;") 'helm-company)

  ;; For some reason company-dabbrev would normally lower the case of
  ;; the completed string.
  (setq company-dabbrev-downcase nil))
