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
                                "~/.emacs.d/elisp/php-extras"
                                "~/.emacs.d/elisp/php-eldoc"
                                "~/.emacs.d/elisp/popup-el"
                                "~/.emacs.d/elisp/coffee-mode"
                                "~/.emacs.d/elisp/web-mode"
                                "~/.emacs.d/elisp/yasnippet"
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

  (setq yas-snippet-dirs '("~/.emacs.d/snippets"))

  ;; Answer just y/n insted of yes or no
  (fset 'yes-or-no-p 'y-or-n-p)

  ;; Don't ask before using narrow commands.
  (put 'narrow-to-region 'disabled nil)
  (put 'narrow-to-page 'disabled nil))
  (put 'dired-find-alternate-file 'disabled nil)

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

  (ido-mode t)
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
