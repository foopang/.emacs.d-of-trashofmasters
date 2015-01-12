(setq cedet-root-path (concat ofc-vendor-dir "/cedet/"))

(require 'cedet-devel-load)

(add-to-list 'load-path (concat cedet-root-path "contrib"))

;; (add-to-list 'semantic-default-submodes 'global-semantic-mru-bookmark-mode)
;; (add-to-list 'semantic-default-submodes 'global-semanticdb-minor-mode)
;; (add-to-list 'semantic-default-submodes 'global-semantic-idle-scheduler-mode)
;; (add-to-list 'semantic-default-submodes 'global-semantic-stickyfunc-mode)
;; (add-to-list 'semantic-default-submodes 'global-cedet-m3-minor-mode)
;; (add-to-list 'semantic-default-submodes 'global-semantic-highlight-func-mode)
;; (add-to-list 'semantic-default-submodes 'global-semanticdb-minor-mode)

;; (global-ede-mode 1)
;; (ede-enable-generic-projects)

(provide 'ofc-cedet)
