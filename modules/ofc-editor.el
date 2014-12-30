(require 'ansi-color)
(require 'uniquify)
(require 'whitespace)

(defun ofc/after-save-hook ()
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

(fringe-mode -1)
(tooltip-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(auto-save-mode -1)
(scroll-bar-mode -1)
(blink-cursor-mode -1)

(global-hl-line-mode +1)
(set-face-attribute hl-line-face nil :underline nil)

(show-paren-mode t)
(column-number-mode t)
(display-battery-mode t)
(delete-selection-mode t)

(setq whitespace-line-column 80) ;; limit line length
(setq whitespace-style '(face tabs empty trailing lines-tail))

(setq-default indent-tabs-mode nil)
(setq-default tab-width 8)
(setq tab-always-indent 'complete)
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

(add-hook 'after-save-hook 'ofc/after-save-hook)

(provide 'ofc-editor)
