(require 'magit)
(require 'git-timemachine)

(set-variable 'magit-emacsclient-executable "/usr/local/Cellar/emacs/HEAD/bin/emacsclient")
(setq magit-status-buffer-switch-function 'switch-to-buffer)

;; Bind magit related commands to prefix `C-x g'.
(global-set-key (kbd "C-x gs") 'magit-status)
(global-set-key (kbd "C-x gd") 'magit-diff)
(global-set-key (kbd "C-x gb") 'magit-branch-manager)
(global-set-key (kbd "C-x gl") 'magit-log-long)
(global-set-key (kbd "C-x gc") 'magit-checkout)
(global-set-key (kbd "C-x gt") 'git-timemachine)

(diminish 'magit-auto-revert-mode)

(provide 'ofc-magit)
