(require 'ggtags)

;; Using a patched version of CTags that understands PHP namespaces
;; and traits.
;; See: github.com/shawncplus/phpcomplete.vim/wiki/Patched-ctags
(setq ggtags-mode-line-project-name nil)
(add-hook 'php-mode-hook (lambda () (ggtags-mode 1)))

(provide 'ofc-ggtags)
