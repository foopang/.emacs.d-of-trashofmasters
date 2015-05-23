;; For the time being I'm using a local fork of EDEP
(require 'edep)

(defun ofc/semantic-php-hook ()
  "Fine-tunes Semantic support for PHP buffers using the EDEP
integration by Joris Steyn"
  (edep-mode)

  (global-semanticdb-minor-mode)
  (global-semantic-idle-scheduler-mode)

  ;; When enabled, Emacs displays a list of possible completions at
  ;; idle time.  The method for displaying completions is given by
  ;; `semantic-complete-inline-analyzer-idle-displayor-class'; the
  ;; default is to show completions inline.
  ;; (global-semantic-idle-completions-mode)
  (global-semantic-idle-breadcrumbs-mode))

(add-hook 'php-mode-hook 'ofc/semantic-php-hook)

(provide 'ofc-edep)
