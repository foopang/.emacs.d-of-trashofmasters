(add-to-list 'load-path "elisp/php-mode")
(add-to-list 'load-path "modules")

(require 'php-mode)
(require 'ofc-semantic-php)

(ert-deftest ofc-semantic-php-semantic-analyse-php-mode-buffers ()
  "`ofc-semantic-php' should enable analysis of php-mode buffers ."
  ;; Open a new php-mode buffer
  ;; Ensure (semantic-active-p)
  (with-temp-buffer
    (php-mode)
    (should (semantic-active-p))))

;; Test the lexer with known inputs
