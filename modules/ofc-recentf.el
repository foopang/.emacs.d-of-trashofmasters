(require 'recentf)

(defun ofc-recentf-exclude-p (file)
  "A predicate to decide whether to exclude FILE from recentf."
  (let ((file-dir (file-truename (file-name-directory file))))
    (-any-p (lambda (dir)
              (string-prefix-p dir file-dir))
            (mapcar 'file-truename (list ofc-savefile-dir package-user-dir)))))

(setq recentf-save-file (expand-file-name "recentf" ofc-savefile-dir)
      recentf-max-saved-items 500
      recentf-max-menu-items 15
      ;; disable recentf-cleanup on Emacs start, because it can cause
      ;; problems with remote files
      recentf-auto-cleanup 'never)

(add-to-list 'recentf-exclude 'ofc-recentf-exclude-p)
;; ignore magit's commit message files
(add-to-list 'recentf-exclude "COMMIT_EDITMSG\\'")

(recentf-mode +1)

(provide 'ofc-recentf)
