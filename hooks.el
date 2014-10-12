;;; Emacs Key Bindings

;;; Created: 2014-09-14
;;;
;;;
;;; Changelog
;;;
;;; 2014-09-14
;;; - First revision of this file.
;;;
;;; 2014-09-14
;;; - Added company mode to php-mode.
;;;
;;; 2014-09-16
;;; - Added require-final-newline to php-mode to ensure all files end with a newline char.
;;;
;;; 2014-09-17
;;; - Added the sql-mode hook to set MySQL syntax highlighting.

(defun ofc/ido-minibuffer-hook nil
  "Ido minibuffer hook"
  (defvar resize-minibuffer-window-max-height)

  (make-local-variable 'resize-minibuffer-window-max-height)
  (setq resize-minibuffer-window-max-height 1))

(defun ofc/sql-mode-hook nil
  "Configure sql-mode"
  (sql-set-product 'mysql))

(defun ofc/php-mode-hook ()
  "A hook to customise the behaviour of php-mode.
   Alters the list of `company-backends' to use ggtags.
   Forces all files to have a final newline.
  "
  (local-set-key (kbd "C-c i") 'company-complete)
  (set (make-local-variable 'ggtags-completing-read-function) nil)
  (set (make-local-variable 'require-final-newline) t)
  (ggtags-mode 1))
