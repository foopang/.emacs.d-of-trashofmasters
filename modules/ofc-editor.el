(require 'ansi-color)
(require 'uniquify)
(require 'whitespace)

(defun ofc/popup-buffer (buffer &optional action norecord)
  "Create a new window at the bottom of the frame and displays
BUFFER in it."
  (let ((win-state (window-state-get))
        (popup-height (floor (/ (frame-height) 1.6180))))

    (delete-other-windows)

    (let ((popup-window (split-window-vertically popup-height)))
      ;; Restore the previous window configuration in the top window
      (window-state-put win-state (frame-first-window))
      (set-window-buffer popup-window buffer))))

(defun ofc/before-save-hook ()
  ""
  (delete-trailing-whitespace))

;; The original `cleanup-buffer' from Magnars,
;; found on Emacs Rocks Episode 12.
;;
;; The companion functions `indent-buffer' and `untabify-buffer' can
;; also be found in Magnar's Emacs configuration files.
;;
;; http://emacsrocks.com/e12.html
;; https://github.com/magnars/.emacs.d
(defun ofc/cleanup-buffer ()
  "Perform a bunch of operations on the whitespace content of a buffer.
Including indent-buffer, which should not be called automatically
on save."
  (interactive)
  (ofc/untabify-buffer)
  (delete-trailing-whitespace)
  (ofc/indent-buffer))

(defun ofc/untabify-buffer ()
  "Performs tab to space conversion across the content of the
whole buffer."
  (interactive)
  (untabify (point-min) (point-max)))

(defun ofc/indent-buffer ()
  "Performs indentation across the content of the whole buffer.
The Emacs functions `point-min' and `point-max' return the first,
respectively the last, accessible position value of point in the
current buffer. Those values change when Narrowing is active.
"
  (interactive)
  (indent-region (point-min) (point-max)))

(defun ofc/join-lines ()
  "Merge two lines."
  (interactive)
  (join-line -1))

(defun ofc/comment-or-uncomment-line-or-region ()
  "Comments or uncomments the current line or region."
  (interactive)
  (if (region-active-p)
      (comment-or-uncomment-region (region-beginning) (region-end))
    (comment-or-uncomment-region (line-beginning-position) (line-end-position))))

(defun ofc/kill-region ()
  "Call the `kill-region' command only with an active mark."
  (interactive)
  (if (use-region-p)
      (kill-region (region-beginning) (region-end))
    (message "Command disabled: mark a region first.")))

(defun ofc/goto-line-with-feedback ()
  "Show line numbers temporarily, while prompting for the line
number input."
  (interactive)
  (unwind-protect
      (progn
        (linum-mode 1)
        (goto-line (read-number "Goto line: ")))
    (linum-mode -1)))

(defun ofc/dot-emacs-autocompile ()
  "Bytecompile the file if is ~/.emacs"
  (interactive)
  (require 'bytecomp)
  (if (string= (buffer-file-name) (expand-file-name (concat default-directory ".emacs")))
      (byte-compile-file (buffer-file-name))))

(defun ofc/colorize-compilation-buffer ()
  "Colorise the eshell buffer with ANSI colors."
  (toggle-read-only)
  (ansi-color-apply-on-region (point-min) (point-max))
  (toggle-read-only))

(defun ofc/startup-hook ()
    "This hook will show the bookmark list as soon as Emacs is
done starting up."
    (bookmark-bmenu-list)
    (split-window-vertically)
    (switch-to-buffer "*Bookmark List*"))

;; Show the initial screen with my bookmarks.
(add-hook 'after-init-hook 'ofc/startup-hook)
(add-hook 'before-save-hook 'ofc/before-save-hook)

;; Bind M-g to temporarily display line numbers when jumping to a
;; line.
(global-set-key (kbd "M-g") 'ofc/goto-line-with-feedback)

;; Bind C-/ to the comment line or region function which does pretty
;; much what it says on the tin.
(global-set-key (kbd "C-w") 'ofc/kill-region)
(global-set-key (kbd "C-/") 'ofc/comment-or-uncomment-line-or-region)

;; Bind C-x a r to align the text in the region.
(global-set-key (kbd "C-x a r") 'align-regexp)

;; Join two consecutive lines into one.
(global-set-key (kbd "M-^") 'ofc/join-lines)

;; Don't kill Emacs without confirmation.
(setq confirm-kill-emacs 'y-or-n-p)

(show-paren-mode t)
(column-number-mode t)
(global-hl-line-mode t)
(display-battery-mode t)
(delete-selection-mode t)

(set-face-attribute hl-line-face nil :underline nil)

(setq bookmark-save-flag t)

(setq whitespace-line-column 80) ;; limit line length
(setq whitespace-style '(face tabs empty trailing lines-tail))

(setq-default indent-tabs-mode nil)
(setq-default tab-width 8)
(setq tab-always-indent t)
(setq tab-width 4)

;; enable narrowing commands
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-defun 'disabled nil)

;; enabled change region case commands
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; enable erase-buffer command
(put 'erase-buffer 'disabled nil)

(put 'dired-find-alternate-file 'disabled nil)

;; Save point position between sessions
(setq-default save-place t)
(setq save-place-file ofc-savefile-dir)

(setq-default cursor-type 'bar)

(setq inhibit-startup-message t
      initial-scratch-message nil
      truncate-lines t
      overflow-newline-into-fringe t

      ;; Use the X11 clipboard
      x-select-enable-clipboard t

      ;; Store all backup and autosave files in the
      ;; temporary files directory.
      backup-directory-alist `((".*" . ,temporary-file-directory))
      auto-save-file-name-transforms `((".*" ,temporary-file-directory t))

      ;; Smooth scrolling settings
      scroll-step 1
      scroll-margin 5
      redisplay-dont-pause t
      scroll-conservatively 10
      scroll-preserve-screen-position 1
      mouse-wheel-scroll-amount '(0.01))

;; Answer just y/n insted of yes or no
(fset 'yes-or-no-p 'y-or-n-p)

(setq uniquify-buffer-name-style 'forward
      uniquify-separator "/"
      ;; rename after killing uniquified
      uniquify-after-kill-buffer-p t
      ;; don't muck with special buffers
      uniquify-ignore-buffers-re "^\\*")

(provide 'ofc-editor)
