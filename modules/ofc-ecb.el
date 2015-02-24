(require 'ecb)

;; Disable the Tip of the Day popup.
(setq ecb-tip-of-the-day nil)
(setq ecb-tree-make-parent-node-sticky nil)

;; ECB Left3 Layout
;; -------------------------------------------------------
;; |              |                                      |
;; |  Directories |                                      |
;; |    ⌘-d       |                                      |
;; |              |                                      |
;; |              |                                      |
;; |--------------|                                      |
;; |              |                                      |
;; |  Sources     |               Edit                   |
;; |    ⌘-s       |               ⌘-e                    |
;; |              |                                      |
;; |--------------|                                      |
;; |              |                                      |
;; |  Methods     |                                      |
;; |    ⌘-m       |                                      |
;; |              |                                      |
;; -------------------------------------------------------
;; |                                                     |
;; |                    Compilation                      |
;; |                        ESC                          |
;; -------------------------------------------------------
;;
(setq ecb-layout-name "left3")

;; In case the Method buffer gets messed up see:
;; http://www.xemacs.org/Documentation/packages/html/ecb_4.html#SEC48

;; Hit ESC to toggle the compile window.
;; I'm used to this because of Quake III and Chrome consoles.
(global-set-key (kbd "<escape>") 'ecb-toggle-compile-window)
(global-set-key (kbd "s-m") 'ecb-goto-window-methods)
(global-set-key (kbd "s-h") 'ecb-goto-window-history)
(global-set-key (kbd "s-s") 'ecb-goto-window-sources)
(global-set-key (kbd "s-d") 'ecb-goto-window-directories)
(global-set-key (kbd "s-e") 'ecb-goto-window-edit-last)

;; Helm is configured to be shown in the compilation window.
;; This appears to be causing random crashes after Emacs has
;; been running with ecb activated for at least a day or so.
(setq ecb-compilation-buffer-names (quote
                                    (("*Calculator*")
                                     ("*vc*")
                                     ("*vc-diff*")
                                     ("*Apropos*")
                                     ("*Occur*")
                                     ("*shell*")
                                     ("\\*[cC]ompilation.*\\*" . t)
                                     ("\\*i?grep.*\\*" . t)
                                     ("*Completions*")
                                     ("*Backtrace*")
                                     ("*Compile-log*")
                                     ("*bsh*")
                                     ("*eshell*"
                                     ("*Messages*")
                                     ("\\*helm[- ].*\\*" . t)))))

;; Activate ECB as soon as Emacs is started.
(ecb-activate)

(provide 'ofc-ecb)
