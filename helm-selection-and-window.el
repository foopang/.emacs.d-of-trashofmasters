;; a hook that makes all helm buffers respect a
;; minimum width (when a window is split horizontally)
;; their height otherwise
(defun ofc/helm-window-hook ()
  (when (< (window-width) 80)
    (window-width 80)))

(add-hook 'helm-update-hook 'ofc/helm-window-hook)
(remove-hook 'helm-update-hook 'ofc/helm-window-hook)

;; split window to show helm buffer in a buffer
;; at the bottom.
(defun ofc/pop-helm-buffer (buffer &optional other-window norecord)
  (interactive (list (read-buffer "Pop to buffer: " (other-buffer))
		     (if current-prefix-arg t)))
  ;; ...
  )

(setq helm-display-function 'pop-to-buffer)
;;(setq helm-display-function 'ofc/pop-helm-buffer)
(window-resize nil 40 t)

(defun ofc/log (message) (message message))

(defun ofc/quit-popup-buffer ()
  (interactive)
  (ofc/log "Quitting bottom window")
  (local-set-key "q" nil)
  (delete-other-windows))

(defun ofc/popup-buffer (buffer &optional action norecord)
  "Create a new window at the bottom of the frame and displays
BUFFER in it."
  (let ((win-state (window-state-get)))

    ;; Restore the previous window configuration in the top window
    (window-state-put win-state (frame-first-window))
    (delete-other-windows)

    (let ((popup-window (split-window-vertically)))
      (select-window popup-window)
      (display-buffer-use-some-window buffer popup-window)
      (local-set-key "q" 'ofc/quit-popup-buffer))))

(setq helm-display-function 'popwin:pop-to-buffer)
(setq helm-display-function 'ofc/popup-buffer)
