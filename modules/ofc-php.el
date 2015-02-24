(require 'php-mode)
(require 'php-extras)

(defun ofc/php-mode-hook ()
  "A custom PHP mode initialisation hook."
  (setq mode-name "PHP")
  (ggtags-mode 1)
  (set (make-local-variable 'require-final-newline) t))

(add-hook 'php-mode-hook 'ofc/php-mode-hook)

;; Activate `flyspell-prog-mode' minor mode during PHP editing to
;; avoid spelling mistakes in comments and strings.
(add-hook 'php-mode-hook 'flyspell-prog-mode)

(push '("\\.php" . php-mode) auto-mode-alist)

(provide 'ofc-php)
