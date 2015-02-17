;; Option is meta (M).
(setq mac-option-modifier 'meta)

;; Fn is meta (M)
(setq ns-function-modifier 'meta)

;; Let the right Alt be used for special characters.
(setq mac-right-option-modifier 'none)

;; Initialise the environment
(exec-path-from-shell-initialize)

(global-unset-key (kbd "s-t"))
(global-unset-key (kbd "s-g"))
(global-unset-key (kbd "s-q"))
(global-unset-key (kbd "s-f"))
(global-unset-key (kbd "s-w"))
(global-unset-key (kbd "s-s"))
(global-unset-key (kbd "s-z"))
(global-unset-key (kbd "s-y"))
(global-unset-key (kbd "s-o"))
(global-unset-key (kbd "s-d"))
(global-unset-key (kbd "s-a"))

(menu-bar-mode 1)

(provide 'ofc-osx)
