(require 'ecb)

;; Create a sequence of 3 ECB layouts. The default layout is
;; doesn't have to be one from the sequence.
;;
;; Default layout: ECB Left3 Layout
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
;; |                                                     |
;; -------------------------------------------------------
;;
;; ECB Left13 Layout
;; -------------------------------------------------------
;; |              |                                      |
;; ~              ~                                      ~
;; | Directories  |               Edit                   |
;; |     ⌘-d      |               ⌘-e                    |
;; ~              ~                                      ~
;; |              |                                      |
;; -------------------------------------------------------
;; |                                                     |
;; |                    Compilation                      |
;; |                        ESC                          |
;; |                                                     |
;; -------------------------------------------------------
;;
;; ECB Left14 Layout
;; -------------------------------------------------------
;; |              |                                      |
;; ~              ~                                      ~
;; |              |                                      |
;; | Directories  |                                      |
;; |     ⌘-d      |                                      |
;; ~              ~                                      ~
;; |              |               Edit                   |
;; |              |               ⌘-e                    |
;; |--------------|                                      |
;; |              |                                      |
;; |  History     |                                      |
;; |    ⌘-h       |                                      |
;; |              |                                      |
;; -------------------------------------------------------
;; |                                                     |
;; |                    Compilation                      |
;; |                        ESC                          |
;; |                                                     |
;; -------------------------------------------------------

(setq ecb-layout-name "left3")
(setq ecb-toggle-layout-sequence (list "left3" "left13" "left14"))

;; Disable the Tip of the Day popup.
(setq ecb-tip-of-the-day nil)
(setq ecb-tree-make-parent-node-sticky nil)

;; Configure ECB to split the frame, instead of the edit window,
;; to make real estate when displaying the compilation window.
(setq ecb-compile-window-width 'frame)
(setq ecb-compile-window-height 11)

;; Show files in the directory tree buffer.
(setq ecb-show-sources-in-directories-buffer 'always)

;; Activate ECB as soon as Emacs is started.
(add-hook 'after-init-hook 'ecb-activate)

;; Hit ESC to toggle the compile window.
(global-set-key (kbd "<escape>") 'ecb-toggle-compile-window)
(global-set-key (kbd "s-m") 'ecb-goto-window-methods)
(global-set-key (kbd "s-h") 'ecb-goto-window-history)
(global-set-key (kbd "s-s") 'ecb-goto-window-sources)
(global-set-key (kbd "s-c") 'ecb-goto-window-compilation)
(global-set-key (kbd "s-d") 'ecb-goto-window-directories)
(global-set-key (kbd "s-e") 'ecb-goto-window-edit-last)
(global-set-key (kbd "s-<return>") 'ecb-toggle-layout)

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
                                     ;; ("\\*magit.*\\*" . t)
                                     ("\\*[cC]ompilation.*\\*" . t)
                                     ("\\*i?grep.*\\*" . t)
                                     ("*Completions*")
                                     ("*Backtrace*")
                                     ("*Compile-log*")
                                     ("*bsh*")
                                     ("*helm*")
                                     ("\\*helm[- ].+\\*" . t))))

(add-hook 'ecb-activate-hook (lambda ()
  (cancel-function-timers 'ecb-stealthy-updates)))


(provide 'ofc-ecb)
