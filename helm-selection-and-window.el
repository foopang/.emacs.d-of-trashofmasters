(defun ofc/popup-buffer (buffer &optional action norecord)
  "Create a new window at the bottom of the frame and displays
BUFFER in it."
  (let ((win-state (window-state-get)))
    (delete-other-windows)
    (let ((popup-window (split-window-vertically)))
      ;; Restore the previous window configuration in the top window
      (window-state-put win-state (frame-first-window))
      (set-window-buffer popup-window buffer))))

(setq helm-display-function 'popwin:pop-to-buffer)
(setq helm-display-function 'ofc/popup-buffer)
