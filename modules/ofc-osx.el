;; Option is meta (M).
(setq mac-option-modifier 'meta)

;; Fn is meta (M)
(setq ns-function-modifier 'meta)

;; Let the right Alt be used for special characters.
(setq mac-right-option-modifier 'none)

(setq use-dialog-box nil)

;; Initialise the environment
(exec-path-from-shell-initialize)

(global-unset-key (kbd "s-t"))
(global-unset-key (kbd "s-u"))
(global-unset-key (kbd "s-l"))
(global-unset-key (kbd "s-j"))
(global-unset-key (kbd "s-x"))
(global-unset-key (kbd "s-c"))
(global-unset-key (kbd "s-v"))
(global-unset-key (kbd "s-p"))
(global-unset-key (kbd "s-g"))
(global-unset-key (kbd "s-q"))
(global-unset-key (kbd "s-f"))
(global-unset-key (kbd "s-n"))
(global-unset-key (kbd "s-m"))
(global-unset-key (kbd "s-,"))
(global-unset-key (kbd "s-w"))
(global-unset-key (kbd "s-s"))
(global-unset-key (kbd "s-S"))
(global-unset-key (kbd "s-D"))
(global-unset-key (kbd "s-z"))
(global-unset-key (kbd "s-y"))
(global-unset-key (kbd "s-o"))
(global-unset-key (kbd "s-d"))
(global-unset-key (kbd "s-a"))

(menu-bar-mode 1)

(provide 'ofc-osx)
