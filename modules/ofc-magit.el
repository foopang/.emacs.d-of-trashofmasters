(require 'magit)
(require 'git-timemachine)

(set-variable 'magit-emacsclient-executable "/usr/local/Cellar/emacs/HEAD/bin/emacsclient")
(setq magit-status-buffer-switch-function 'switch-to-buffer)

;; Bind magit related commands to prefix `C-x g'.
(global-set-key (kbd "C-c g s") 'magit-status)
(global-set-key (kbd "C-c g d") 'magit-diff)
(global-set-key (kbd "C-c g b") 'magit-branch-manager)
(global-set-key (kbd "C-c g l") 'magit-log-long)
(global-set-key (kbd "C-c g c") 'magit-checkout)
(global-set-key (kbd "C-c g t") 'git-timemachine)

(provide 'ofc-magit)
