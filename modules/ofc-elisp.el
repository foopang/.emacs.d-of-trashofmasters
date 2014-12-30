(defun ofc/elisp-hook ()
  ""
  (setq mode-name "elisp"))

(add-hook 'emacs-lisp-hook 'ofc/elisp-hook)

(provide 'ofc-elisp)
