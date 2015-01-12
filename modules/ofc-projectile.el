(require 'projectile)

(projectile-global-mode)
;;(setq projectile-tags-command "/usr/local/bin/ctags -Re --fields=+aimS --languages=php")
(setq projectile-mode-line '(:eval (format " (%s)" (projectile-project-name))))

(provide 'ofc-projectile)
