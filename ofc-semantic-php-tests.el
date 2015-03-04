(add-to-list 'load-path "elisp/php-mode")
(add-to-list 'load-path "modules")

(require 'php-mode)
(require 'ofc-semantic-php)

(defun ofc-have-token-in-stream (token token-stream)
  "Searches for TOKEN in TOKEN-STREAM and returns t if it finds it.
If TOKEN is not found then nil is returned."
  (when (member (list token) (mapcar (lambda (x) (list (car x))) token-stream))
    t))

(ert-deftest ofc-semantic-php-semantic-analyse-php-mode-buffers ()
  "A hook is added to php-mode to enable Semantic analysis."
  (with-temp-buffer
    (php-mode)
    (should (semantic-active-p))))

(ert-deftest ofc-semantic-php-lex-open-tag-in-empty-file ()
  "Ensure parsing the T_OPEN_TAG in an empty file"
  (with-temp-buffer
    (php-mode)
    (insert "<?php")
    (semantic-lex-init)
    (setq semantic-lex-analyzer 'ofc-semantic-php-lexer)
    (let ((token-stream (semantic-lex (point-min) (point-max))))
      (should (equal '((T_OPEN_TAG 1 . 6)) token-stream)))))
