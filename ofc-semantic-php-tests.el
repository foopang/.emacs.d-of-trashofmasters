(add-to-list 'load-path "elisp/php-mode")
(add-to-list 'load-path "modules")

(require 'php-mode)
(require 'ofc-semantic-php)
(require 'ofc-semantic-php-wy)
(require 'ofc-semantic-php-wy-lex)

(ert-deftest ofc-semantic-php-semantic-analyse-php-mode-buffers ()
  "A hook is added to php-mode to enable Semantic analysis."
  (with-temp-buffer
    (php-mode)
    (should (semantic-active-p))))

;; Test Checklist:
;; unit test the parser
;; - test the bloody qualified name parsing.
;; test tag detection
