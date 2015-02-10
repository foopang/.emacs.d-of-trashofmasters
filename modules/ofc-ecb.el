(require 'ecb)

;; Disable the Tip of the Day popup.
(setq ecb-tip-of-the-day nil)
(setq ecb-tree-make-parent-node-sticky nil)

;; Activate ECB as soon as Emacs is started.
(ecb-activate)

;; Hit ESC to toggle the compile window.
;; I'm used to this because of Quake III and Chrome consoles.
(global-set-key (kbd "<escape>") 'ecb-toggle-compile-window)

(provide 'ofc-ecb)
