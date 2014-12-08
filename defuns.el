;;; .emacs --- Andrea Turso Emacs configuration file

;;; Created: 2012-09-04
;;;
;;; Changelog
;;;
;;; 2014-10-10
;;; - Added a procedure to use in conjunction with `compilation-filter-hook'
;;; to colorise compilation output.
;;;
;;; 2014-10-14
;;; - Set `magit-emacsclient-executable' to point to the brew installation.
;;;
;;; 2014-10-21
;;; - Added the ofc/kill-ring to kill a region only when a mark is active.
;;;
;;; 2014-10-31
;;; - Added popwin and changed helm to show its buffer in a popup window.

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

(defun ofc/custom nil
  "Andrea's customisations"

  (setq popwin:special-display-config
        '(
          ("*Help*" :height 30 :stick t)
          ("*Completions*" :noselect t)
          ("*compilation*" :noselect t)
          ("*Messages*")
          ("*Occur*" :noselect t)
          ("*shell*" :height 30)
          ("*Shell Command Output*" :noselect t)
          ("*Kill Ring*" :height 40)
          ("*Compile-Log" :height 20 :stick t)
          ("*Ack-and-a-half*" :height 20)))

  (setq mouse-wheel-scroll-amount '(0.01))

  ;; Set username and email address
  (setq user-full-name "Andrea Turso"
        user-mail-address "trashofmasters@gmail.com")

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

(defun ofc/kill-region ()
  "Call kill-region only when a mark is active."
  (interactive)
  (if (use-region-p)
      (kill-region (region-beginning) (region-end))
    (message "Mark a region first.")))

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

(defun ofc/yasnippet nil
  "Install yasnippet"
  (setq yas-snippet-dirs '("~/.emacs.d/snippets"))
  (yas-global-mode 1))

(defun ofc/helm nil
  "Customise Helm"
  (setq helm-display-function 'popwin:popup-buffer)
  ;; Enable projectile to use projectile-helm integration.
  (setq projectile-completion-system 'helm
        projectile-switch-project-action 'helm-projectile)

  (helm-mode 1))

(defun ofc/company nil
  "Customise Company mode"

  ;; For some reason company-dabbrev would normally lower the case of
  ;; the completed string.
  (setq company-dabbrev-downcase nil))

(defun ofc/colorize-compilation-buffer nil
  (toggle-read-only)
  (ansi-color-apply-on-region (point-min) (point-max))
  (toggle-read-only))

(defun ofc/run-phpunit nil
  "Scan the parent directories until a phpunit configuration file is found and run the phpunit command."
  (interactive)
  (with-temp-buffer
    (while (and (not (file-exists-p "phpunit.xml"))
                (not (file-exists-p "phpunit.xml.dist"))
                (not (equal "/" default-directory)))
      (cd ".."))
    (call-interactively 'compile)))

(defun ofc/magit ()
  "Configures Magit mode."
  (set-variable 'magit-emacsclient-executable "/usr/local/Cellar/emacs/HEAD/bin/emacsclient")
  (setq magit-status-buffer-switch-function 'switch-to-buffer))

(defun ofc/ack ()
  "Configure ack"
  (setq ack-and-a-half-executable "/usr/local/bin/ack")
  (defalias 'ack 'ack-and-a-half))
