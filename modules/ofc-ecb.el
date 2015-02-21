(require 'ecb)

;; Disable the Tip of the Day popup.
(setq ecb-tip-of-the-day nil)
(setq ecb-tree-make-parent-node-sticky nil)

;; Activate ECB as soon as Emacs is started.
(ecb-activate)

;; Hit ESC to toggle the compile window.
;; I'm used to this because of Quake III and Chrome consoles.
(global-set-key (kbd "<escape>") 'ecb-toggle-compile-window)
(global-set-key (kbd "s-m") 'ecb-goto-window-methods)
(global-set-key (kbd "s-h") 'ecb-goto-window-history)
(global-set-key (kbd "s-s") 'ecb-goto-window-sources)
(global-set-key (kbd "s-d") 'ecb-goto-window-directories)
(global-set-key (kbd "s-e") 'ecb-goto-window-edit-last)

(provide 'ofc-ecb)
