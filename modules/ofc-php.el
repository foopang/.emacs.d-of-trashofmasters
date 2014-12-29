(require 'php-mode)
(require 'php-extras)

(add-hook 'php-mode-hook (lambda ()
  (local-set-key (kbd "C-c i") 'company-complete)
  (set (make-local-variable 'ggtags-completing-read-function) nil)
  (set (make-local-variable 'require-final-newline) t)))

(push '("\\.php" . php-mode) auto-mode-alist)

(provide 'ofc-php)
