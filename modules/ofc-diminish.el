(require 'diminish)

;; I haven't yet figured why helm and magit require
;; diminish to be quoted when the rest seem to work
;; just fine.
(eval-after-load "helm-mode"
  '(diminish 'helm-mode))

(eval-after-load "magit"
  '(diminish 'magit-auto-revert-mode))

(eval-after-load "abbrev-mode"
  '(diminish 'abbrev-mode))

(eval-after-load "company-mode"
  '(diminish 'company-mode " Cm"))

(eval-after-load "yas-minor-mode"
  '(diminish 'yas-minor-mode " Ys"))

(eval-after-load "ggtags-mode"
  '(diminish 'ggtags-mode " Gt"))

(eval-after-load "ggtags-mode"
  '(diminish 'ggtags-mode " Gt"))

(provide 'ofc-diminish)
