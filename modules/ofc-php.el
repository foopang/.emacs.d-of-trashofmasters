(require 'php-mode)
(require 'php-extras)

(defun ofc/php-mode-hook ()
  ""
  (setq mode-name "PHP")
  (set (make-local-variable 'require-final-newline) 'visit-save))

(add-hook 'php-mode-hook 'ofc/php-mode-hook)

(push '("\\.php" . php-mode) auto-mode-alist)

(provide 'ofc-php)
