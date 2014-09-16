;;; Emacs Key Bindings

;;; Created: 2014-09-14
;;;
;;;
;;; Changelog
;;;
;;; - 2014-09-14
;;;   First revision of this file.

;; Custom global key bindings
(defun ofc/keybindings nil
  (global-unset-key (kbd "C-x C-b"))

  (when (fboundp 'ofc/goto-line-with-feedback)
    (global-set-key [remap goto-line] 'ofc/goto-line-with-feedback))

  (when window-system
    (global-set-key (kbd "C-x C-c") 'ofc/prompt-before-closing))

  (when (fboundp 'sr-speedbar-toggle)
    (global-set-key (kbd "M-§") 'sr-speedbar-toggle))

  (when (fboundp 'comment-or-uncomment-line-or-region)
    (global-set-key (kbd "C-/") 'comment-or-uncomment-line-or-region))

  (when (fboundp 'ofc/rotate-windows)
      (global-set-key (kbd "C-c C-r") 'ofc/rotate-windows))

  (global-set-key (kbd "C-c C-a") 'mark-whole-buffer)
  (global-set-key (kbd "C-x a r") 'align-regexp))

(setq yas-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-o") 'yas-expand)
    map))
