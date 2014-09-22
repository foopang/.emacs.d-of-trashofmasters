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

;; Custom global key bindings
(defun ofc/keybindings nil
  (global-unset-key (kbd "C-x C-b"))

  (setq yas-minor-mode-map
        (let ((map (make-sparse-keymap)))
          (define-key map (kbd "C-o") 'yas-expand)
          map))

  (when (eq system-type 'darwin)
    (progn
      ;; Option is meta (M).
      (setq mac-option-modifier 'meta)

      ;; Command is super (S).
      (setq mac-command-modifier 'none)

      ;; Let the right Alt be used for special characters.
      (setq mac-right-option-modifier 'none)))

  (when (fboundp 'ofc/goto-line-with-feedback)
    (global-set-key (kbd "M-g") 'ofc/goto-line-with-feedback))

  (when window-system
    (global-set-key (kbd "C-x C-c") 'ofc/prompt-before-closing))

  (when (fboundp 'sr-speedbar-toggle)
    (global-set-key (kbd "§") 'sr-speedbar-toggle))

  (when (fboundp 'comment-or-uncomment-line-or-region)
    (global-set-key (kbd "C-/") 'comment-or-uncomment-line-or-region))

  (when (fboundp 'magit-status)
    (global-set-key (kbd "C-x g") 'magit-status))

  (global-set-key (kbd "<escape>") 'keyboard-quit)

  (global-set-key (kbd "C-<return>") 'highlight-symbol-at-point)
  (global-set-key (kbd "M-<return>") 'highlight-symbol-query-replace)

  (global-set-key (kbd "C-x C-m") 'execute-extended-command)

  (global-set-key (kbd "<down>") 'highlight-symbol-next)
  (global-set-key (kbd "<up>") 'highlight-symbol-prev)

  (global-set-key (kbd "C-c C-a") 'mark-whole-buffer)
  (global-set-key (kbd "C-x a r") 'align-regexp))
