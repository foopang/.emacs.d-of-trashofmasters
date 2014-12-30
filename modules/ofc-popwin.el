(require 'popwin)

(setq popwin:special-display-config nil)
;; (push '("^\*helm .+\*$" :regexp t :height 40 :dedicated t) popwin:special-display-config)
;; (push '("^\*helm-.+\*$" :regexp t :height 40 :dedicated t) popwin:special-display-config)
;;(push '("*ggtags-global*" :position left :dedicated t ) popwin:special-display-config)

(popwin-mode 1)

(provide 'ofc-popwin)
