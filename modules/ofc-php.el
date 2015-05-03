(require 'php-mode)
(require 'php-extras)
(require 'ofc-comint-php)
(require 'ofc-edep)

(defun ofc/php-mode-hook ()
  "A custom PHP mode initialisation hook."
  (setq mode-name "PHP")
  (local-set-key (kbd "M-n") 'company-complete)
  (setq php-lineup-cascaded-calls t)
  (set (make-local-variable 'require-final-newline) t))

;; Activate the `ofc/php-mode-hook' to run my setup my customisations
;; to php-mode.
(add-hook 'php-mode-hook 'ofc/php-mode-hook)

;; Activate `flyspell-prog-mode' minor mode during PHP editing to
;; avoid spelling mistakes in comments and strings.
(add-hook 'php-mode-hook 'flyspell-prog-mode)

;; Activate the `ctags-auto-update-mode' which recreates the TAGS file
;; with exuberant ctags.
;;
;; This function is available since the introduction the
;; helm-etags-plus package.
(add-hook 'php-mode-hook 'ctags-auto-update-mode)

;; Use php-mode to visiting .php files.
(push '("\\.php" . php-mode) auto-mode-alist)

(provide 'ofc-php)
