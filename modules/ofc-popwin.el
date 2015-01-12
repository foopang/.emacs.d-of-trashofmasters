(require 'popwin)

(popwin-mode 1)

(push '("^\*helm .+\*$" :regexp t) popwin:special-display-config)
(push '("^\*helm-.+\*$" :regexp t) popwin:special-display-config)

(provide 'ofc-popwin)
