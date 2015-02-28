(require 'projectile)

(projectile-global-mode)
(setq projectile-mode-line '(:eval (format " <%s>" (projectile-project-name))))

(provide 'ofc-projectile)
