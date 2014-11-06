;;; Emacs Key Bindings

;;; Created: 2014-09-14
;;;
;;;
;;; Changelog
;;;
;;; 2014-09-14
;;; - First revision of this file.
;;; - C-c C-a marks the whole buffer (formerly C-x h).
;;;
;;; 2014-09-17
;;; - Bound C-o to yas-expand.
;;;
;;; 2014-09-18
;;; - Bound C-RET will now highlight symbols.
;;; - Up/Down arrows will navigate highlighted symbols.
;;;
;;; 2014-09-22
;;; - Bound C-x g to magit-status.
;;; - Bound C-x C-m to execute-extended-command (formerly M-x).
;;; - Inhibited Command key on Mac, Option is M(eta).
;;; - Right Option key should now let me input special chars on Mac.
;;; - Mapped M-g to goto-line-with-feedback.
;;; - Bound ESC to keyboard-quit (aka C-g).
;;;
;;; 2014-09-25
;;; - Bound C-x C-b (and C-x C-a, to see which binding is easier to
;;;   hit) to helm-mini. Helm-mini is a buffer switcher.
;;;
;;; 2014-10-12
;;; - Removed sr-speedbar key binding.
;;;
;;; 2014-10-13
;;; - Bound H-SPC to show project buffers with projectile.
;;; - Bound H-` to switch between frames (consistently with Mac change window)
;;;
;;; 2014-10-21
;;; - Unbound C-z and C-x C-z.
;;; - Bound C-w to `ofc/kill-region' which kills only active regions.
;;;
;;; 2014-11-06
;;; - Unbound key `C-c a` to select all text in the buffer.
;;; - Unbound key to compose a message `C-x m'
;;; - Bound `helm-projectile-switch-to-buffer' to `C-x m'
;;; - Bound various magit commands to the prefix `C-x g':
;;;   - `s' status
;;;   - `b' branch manager
;;;   - `c' checkout
;;;   - `d' diff
;;;   - `t' time machine
;;; - Bound `helm-show-kill-ring' to `C-x y'.

;; Custom global key bindings
(defun ofc/keybindings nil
  ;; Avoid accidentally suspending Emacs.
  (global-unset-key (kbd "C-z"))
  (global-unset-key (kbd "C-x C-z"))
  (global-unset-key (kbd "C-x m"))
  (global-unset-key (kbd "<left>"))
  (global-unset-key (kbd "<right>"))

  (when (eq system-type 'darwin)
    (progn

      ;; Command is hyper (H)
      (setq mac-command-modifier 'hyper)

      ;; Bind H-` to switch between frames.
      (define-key global-map (kbd "H-`") 'other-frame)

      ;; Option is meta (M).
      (setq mac-option-modifier 'meta)

      ;; Let the right Alt be used for special characters.
      (setq mac-right-option-modifier 'none)))

  ;; Bind M-g to temporarily display line numbers when jumping to a
  ;; line.
  (when (fboundp 'ofc/goto-line-with-feedback)
    (global-set-key (kbd "M-g") 'ofc/goto-line-with-feedback))

  ;; Prompt before exiting, we don't want to abruptly terminate the
  ;; development session.
  (when window-system
    (global-set-key (kbd "C-x C-c") 'ofc/prompt-before-closing))

  ;; Bind C-/ to the comment line or region function which does pretty
  ;; much what it says on the tin.
  (when (fboundp 'comment-or-uncomment-line-or-region)
    (global-set-key (kbd "C-/") 'comment-or-uncomment-line-or-region))

  ;; Bind this modified version of `kill-region' which doesn't act
  ;; unless there an active mark.
  (global-set-key (kbd "C-w") 'ofc/kill-region)

  ;; Bind magit related commands to prefix `C-x g'.
  (global-set-key (kbd "C-x g s") 'magit-status)
  (global-set-key (kbd "C-x g d") 'magit-diff)
  (global-set-key (kbd "C-x g b") 'magit-branch-manager)
  (global-set-key (kbd "C-x g c") 'magit-checkout)
  (global-set-key (kbd "C-x g t") 'git-timemachine)

  ;; Treat ESC just like C-g.
  (global-set-key (kbd "<escape>") 'keyboard-quit)

  ;; Use Up/Down arrows to navigate the occurrences of the symbol
  ;; under the cursor across the current buffer.
  (global-set-key (kbd "<down>") 'highlight-symbol-next)
  (global-set-key (kbd "<up>") 'highlight-symbol-prev)

  ;; C-RET will highlight all occurrences of the symbol under the
  ;; cursor across the current buffer.
  (global-set-key (kbd "C-<return>") 'highlight-symbol-at-point)

  ;; M-RET will initiate the interactive replacement of all symbols
  ;; matching the one under the cursor across the current buffer.
  (global-set-key (kbd "M-<return>") 'highlight-symbol-query-replace)

  ;; Use helm-mini to switch between open buffer and files, or
  ;; recently closed ones.
  ;; Bind other Helm commands.
  (global-set-key (kbd "C-x m") 'helm-projectile-switch-to-buffer)
  (global-set-key (kbd "C-x y") 'helm-show-kill-ring)
  (global-set-key (kbd "C-x f") 'helm-occur)
  (global-set-key (kbd "C-x b") 'helm-mini)
  (global-set-key (kbd "C-x C-b") 'helm-mini)

  ;; Duplicate the M-x binding to C-x C-m for easier access.
  (global-set-key (kbd "C-x C-m") 'execute-extended-command)

  ;; Bind C-x a r to align the text in the region.
  (global-set-key (kbd "C-x a r") 'align-regexp))
