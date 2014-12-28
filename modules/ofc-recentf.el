(require 'recentf)

(setq recentf-save-file (expand-file-name "recentf" ofc-savefile-dir)
      recentf-max-saved-items 500
      recentf-max-menu-items 15
      ;; disable recentf-cleanup on Emacs start, because it can cause
      ;; problems with remote files
      recentf-auto-cleanup 'never)

(provide 'ofc-recentf)
