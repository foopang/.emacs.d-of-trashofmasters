(require 'ggtags)

(add-hook 'php-mode-hook (lambda () (ggtags-mode 1)))

(provide 'ofc-ggtags)
