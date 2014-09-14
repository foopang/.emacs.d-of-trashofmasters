;;; Emacs Key Bindings

;;; Created: 2014-09-14
;;;
;;;
;;; Changelog
;;;
;;; - 2014-09-14
;;;   First revision of this file.

(defun ofc/ido-minibuffer-hook nil
  "Ido minibuffer hook"
  (defvar resize-minibuffer-window-max-height)

  (make-local-variable 'resize-minibuffer-window-max-height)
  (setq resize-minibuffer-window-max-height 1))

(defun ofc/php-mode-hook nil
  "Configure php-mode"
  (company-mode))
