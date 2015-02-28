(require 'ggtags)

;; I'm using a Exuberant Ctags 5.8 patched to support the syntax
;; of PHP 5.4 installed as described in the following link.
;;
;; https://github.com/shawncplus/phpcomplete.vim/wiki/Patched-ctags
;;
;; The following environment variables are required to setup ctags
;; as a parser backend to GNU Global.
;;
;; http://www.gnu.org/software/global/manual/global.html#Plug_002din
;;
;; A global, or local, ctags configuration file is then used to always
;; use the `-e' option to produce tag files compatible with Emacs.
;;
(setenv "GTAGSLABEL" "ctags")
(setenv "GTAGSCONF" "/usr/local/share/gtags/gtags.conf")

(setq ggtags-mode-line-project-name nil)

(diminish 'ggtags-mode " Gt")

(provide 'ofc-ggtags)
