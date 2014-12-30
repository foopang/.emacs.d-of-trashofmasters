;;; Emacs Key Bindings

;; Avoid accidentally suspending Emacs.
(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-x C-z"))
(global-unset-key (kbd "C-x m"))
(global-unset-key (kbd "<up>"))
(global-unset-key (kbd "<down>"))
(global-unset-key (kbd "<left>"))
(global-unset-key (kbd "<right>"))

;; Treat ESC just like C-g.
(global-set-key (kbd "<escape>") 'keyboard-quit)

;; Duplicate the M-x binding to C-x C-m for easier access.
(global-set-key (kbd "C-x C-m") 'execute-extended-command)
