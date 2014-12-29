;;; Emacs Key Bindings

;; Avoid accidentally suspending Emacs.
(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-x C-z"))
(global-unset-key (kbd "C-x m"))
(global-unset-key (kbd "<left>"))
(global-unset-key (kbd "<right>"))

;; Treat ESC just like C-g.
(global-set-key (kbd "<escape>") 'keyboard-quit)

;; Use Up/Down arrows to navigate the occurrences of the symbol
;; under the cursor across the current buffer.
(global-set-key (kbd "<up>") 'highlight-symbol-prev)
(global-set-key (kbd "<down>") 'highlight-symbol-next)

;; C-RET will highlight all occurrences of the symbol under the
;; cursor across the current buffer.
(global-set-key (kbd "C-<return>") 'highlight-symbol-at-point)

;; M-RET will initiate the interactive replacement of all symbols
;; matching the one under the cursor across the current buffer.
(global-set-key (kbd "M-<return>") 'highlight-symbol-query-replace)

;; Duplicate the M-x binding to C-x C-m for easier access.
(global-set-key (kbd "C-x C-m") 'execute-extended-command)
