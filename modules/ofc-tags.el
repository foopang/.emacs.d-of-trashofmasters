(require 'helm-etags+)
(require 'ctags-update)

;; Don't pester me with confirmation dialogs in the minibuffer
;; before regenerating the bloody tags.
(setq tags-revert-without-query t)

(defvar ofc-tags-search-buffers nil
  "The history of buffers which initiated the tag search.")

(defun ofc/searching-p nil
  "A predicate used to test the presence of buffers in the
`ofc-buffer-history'."
  (if ofc-buffer-history
      t
    nil))

(defun ofc/tags-find-at-point ()
  "Use some library to find the definition of the symbol at
point."
  (interactive)
  (condition-case error-message
      (let ((initial-buffer (current-buffer)))
        (helm-etags+-select)
        (push initial-buffer ofc-buffer-history))
    (error error-message)))

(defun ofc/tags-stop-search ()
  "Exits from a tag search and return to the buffer which
initiated it.

CAVEATS Currently the switch won't happen unless until you press
M-* to clear the killed buffers in the history."
  (interactive)
  (if (ofc/searching-p)
      (condition-case error-message
          (switch-to-buffer (pop ofc-buffer-history))
        (error (message "A visited definition file was killed.")))
    (message "No tag search in progress.")))

(defun ofc/history-go-back-dwim ()
  "Calls helm etags+ history back command only if we jumped to
a definition"
  (interactive)
  (if (ofc/searching-p)
      (helm-etags+-history-go-back)
    (message "No tag search in progress.")))

(defun ofc/history-go-forward-dwim ()
  "Calls helm etags+ history forward command only if we jumped to
a definition"
  (interactive)
  (if (ofc/searching-p)
      (helm-etags+-history-go-forward)
    (message "No tag search in progress.")))

;; Change the name of the auto update mode without
;; using diminish.
(setq ctags-update-lighter " CtU+")

;; TODO Set these keys only for progn-mode derived modes.
;; (global-set-key "\M-." 'helm-etags+-select)
(global-set-key "\M-." 'ofc/tags-find-at-point)

;;list all visited tags
(global-set-key "\M-*" 'ofc/tags-stop-search)

;;go back directly
(global-set-key "\M-[" 'ofc/history-go-back-dwim)

;;go forward directly
(global-set-key "\M-]" 'ofc/history-go-forward-dwim)

;; Regenerate tags manually.
;; C-u to create a new TAGS file.
;; C-u C-u to copy the ctags command into the kill ring.
(global-set-key "\C-cE" 'ctags-update)

(provide 'ofc-tags)
