(if (executable-find "psysh")
    (setq php-cli-path (executable-find "psysh")
          comint-prompt-regexp "^>>> *")
  (setq php-cli-path (executable-find "php")
        comint-prompt-regexp "^php +> *"))

(setq comint-use-prompt-regexp t)
(setq comint-prompt-read-only t)
(setq php-cli-arguments '())

(defun run-php ()
  "Run an inferior instance of `php' inside Emacs."
  (interactive)
  (let ((php-program php-cli-path)
        (php-switches php-cli-arguments)
        (buffer (comint-check-proc "*php-repl*")))

    ;; create the comint process if there is no buffer.
    (unless buffer
      (apply 'make-comint-in-buffer "php" "*php-repl*" php-program nil php-switches))
    (pop-to-buffer "*php-repl*")))

(defun ofc/php-mode-comint-hook ()
  ""
  (local-set-key (kbd "C-x C-r") 'run-php))

(add-hook 'php-mode-hook 'ofc/php-mode-comint-hook)

(provide 'ofc-comint-php)
