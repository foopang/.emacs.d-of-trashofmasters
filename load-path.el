;;; .emacs --- Andrea Turso Emacs configuration file

;;; Created: 2012-09-04
;;;
;;; Changelog
;;;
;;; 2014-11-06
;;; - Created the load-path file to hold all paths configuration.
;;; - Added git-timemachine.

(defun ofc/load-path nil
  "Configure emacs autoload paths"

  (add-to-list 'custom-theme-load-path "~/.emacs.d/elisp/themes")
  (add-to-list 'custom-theme-load-path "~/.emacs.d/elisp/moe-theme")

  (setq load-path (append (list "~/.emacs.d/elisp/php-mode"
                                "~/.emacs.d/elisp/php-eldoc"
                                "~/.emacs.d/elisp/php-extras"
                                "~/.emacs.d/elisp/web-mode"
                                "~/.emacs.d/elisp/coffee-mode"
                                "~/.emacs.d/elisp/popwin"
                                "~/.emacs.d/elisp/projectile"
                                "~/.emacs.d/elisp/helm"
                                "~/.emacs.d/elisp/git-timemachine"
                                "~/.emacs.d/elisp/moe-theme"
                                "~/.emacs.d/elisp/zenburn-theme"
                                "~/.emacs.d/elisp/yasnippet"
                                "~/.emacs.d/elisp/helm-ack"
                                "~/.emacs.d/elisp/ggtags"
                                "~/.emacs.d/elisp/highlight-symbol"
                                "~/.emacs.d/elisp/sr-speedbar")
                          load-path))

  ;;
  (setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                           ("marmalade" . "http://marmalade-repo.org/packages/")
                           ("melpa" . "http://melpa.milkbox.net/packages/"))))
