(require 'helm-config)
(require 'helm-projectile)

;; Apparently `helm-M-x' hangs...
;;(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "M-x") 'execute-extended-command)
(global-set-key (kbd "C-M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-x f") 'helm-recentf)
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "C-x C-b") 'helm-mini)

;; I'm currently relying on ECB Compile Window support to
;; display Helm buffers at the bottom of the frame.
(setq helm-display-function 'pop-to-buffer)

(setq projectile-completion-system 'helm
      projectile-completion-system 'grizzl
      projectile-switch-project-action 'helm-projectile-find-file)

(helm-mode 1)
(helm-autoresize-mode 1)
(helm-projectile-on)

(provide 'ofc-helm)
