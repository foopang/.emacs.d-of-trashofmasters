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

(ert-deftest ofc-semantic-php-open-tag-in-empty-file ()
  "Ensure parsing the T_OPEN_TAG in a empty PHP file"
  (with-temp-buffer
    (php-mode)
    (erase-buffer)
    (insert "<?php")
    (setq semantic-lex-analyzer 'ofc-semantic-php-lexer)
    (semantic-lex-init)
    (let ((token-stream (semantic-lex-buffer)))
      (should (equal '((T_OPEN_TAG 1 . 6))
                     token-stream)))))

(ert-deftest ofc-semantic-php-open-and-close ()
  "Ensure parsing of a empty PHP file."
  (with-temp-buffer
    (erase-buffer)
    (php-mode)
    (insert "<?php ?>")
    (setq semantic-lex-analyzer 'ofc-semantic-php-lexer)
    (semantic-lex-init)
    (let ((token-stream (semantic-lex-buffer)))
      (should (equal '((T_OPEN_TAG 1 . 7)
                       (T_CLOSE_TAG 7 . 9))
                     token-stream)))))

(ert-deftest ofc-semantic-php-top-level-namespace-declaration ()
  "Ensure the parsing of top-level namespace declartion"
  (with-temp-buffer
    (erase-buffer)
    (php-mode)
    (insert "<?php namespace Foo;")
    (setq semantic-lex-analyzer 'ofc-semantic-php-lexer)
    (semantic-lex-init)
    (let ((token-stream (semantic-lex-buffer)))
      (should (equal '((T_OPEN_TAG 1 . 7)
                       (T_NAMESPACE 7 . 16)
                       (T_STRING 17 . 20)
                       (T_SEMICOLON 20 . 21))
                     token-stream)))))

(ert-deftest ofc-semantic-parse-unqualified-names-in-namespace-declarations ()
  "Ensure the parsing of unqualified names in namespace declarations.
For example `namespace Foo;` or `namespace Bar {}`."
  (with-temp-buffer
    (erase-buffer)
    (insert "<?php namespace Foo; namespace Bar {};")
    (ofc/semantic-php-setup)
    (php-mode)
    ;; I'm trying to assert against the tags extracted from
    ;; the parser, rather than the tokens from the lexer.
    (should (semantic-create-imenu-index))))

;; Test Checklist:
;; unit test the parser
;; - test the bloody qualified name parsing.
;; test tag detection
