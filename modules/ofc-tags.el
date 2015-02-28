(require 'helm-etags+)
(require 'ctags-update)

(global-set-key "\C-cE" 'ctags-update)

(eval-after-load "ctags-auto-update-mode" '(diminish 'ctags-auto-update-mode " Ct+"))

(provide 'ofc-tags)
