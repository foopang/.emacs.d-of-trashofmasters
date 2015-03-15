(add-to-list 'load-path "elisp/php-mode")
(add-to-list 'load-path "modules")

(require 'php-mode)
(require 'ofc-semantic-php)
(require 'ofc-semantic-php-wy)
(require 'ofc-semantic-php-wy-lex)

(defun ofc/lex-one-php-token (text)
  "Lexes a piece of TEXT into a single PHP token.
Only the first token is returned, so if TEXT happens to produce
more then one, the rest will be discarded."
  (with-temp-buffer
    (setq semantic-lex-analyzer 'ofc-semantic-php-lexer)
    (semantic-lex-init)
    (erase-buffer)
    (insert text)
    (car (ofc/lex-php-tokens text))))

(defun ofc/lex-php-tokens (text)
  "Lexes a piece of TEXT into a list of tokens."
  (with-temp-buffer
    (setq semantic-lex-analyzer 'ofc-semantic-php-lexer)
    (semantic-lex-init)
    (erase-buffer)
    (insert text)
    ;; strips out the position of the token so that
    ;; (T_STRING 1 . 3) becomes T_STRING so that
    ;; we can return a list of tokens like
    ;; (T_ECHO T_STRING T_SEMICOLON)
    (mapcar (lambda (elm) (car elm)) (semantic-lex-buffer))))

(ert-deftest ofc-semantic-php-lex-php-short-open-tag ()
  "Lexes a short open tag to T_OPEN_TAG"
  (should (equal 'T_OPEN_TAG (ofc/lex-one-php-token "<?"))))

(ert-deftest ofc-semantic-php-lex-php-open-tag ()
  "Lexes a regular open tag to T_OPEN_TAG"
  (should (equal 'T_OPEN_TAG (ofc/lex-one-php-token "<?php"))))

(ert-deftest ofc-semantic-php-lex-php-close-tag ()
  "Lexes a closing tag to T_CLOSE_TAG"
  (should (equal 'T_CLOSE_TAG (ofc/lex-one-php-token "?>"))))

(ert-deftest ofc-semantic-php-lex-integer ()
  "Lexes a integer keyword to T_NUMBER"
  (should (equal 'T_NUMBER (ofc/lex-one-php-token "1"))))

(ert-deftest ofc-semantic-php-lex-float ()
  "Lexes a float keyword to T_NUMBER"
  (should (equal 'T_NUMBER (ofc/lex-one-php-token "1.0"))))

(ert-deftest ofc-semantic-php-lex-string-in-single-quotes ()
  "Lexes a string in single quotes to T_CONSTANT_ENCAPSED_STRING"
  (should (equal 'T_CONSTANT_ENCAPSED_STRING (ofc/lex-one-php-token "'hello'"))))

(ert-deftest ofc-semantic-php-lex-string-in-double-quotes ()
  "Lexes a string in double quotes to T_CONSTANT_ENCAPSED_STRING"
  (should (equal 'T_CONSTANT_ENCAPSED_STRING (ofc/lex-one-php-token "\"hello\""))))

(ert-deftest ofc-semantic-php-lex-use ()
  "Lexes a use keyword to T_USE"
  (should (equal 'T_USE (ofc/lex-one-php-token "use"))))

(ert-deftest ofc-semantic-php-lex-namespace ()
  "Lexes a namespace keyword to T_NAMESPACE"
  (should (equal 'T_NAMESPACE (ofc/lex-one-php-token "namespace"))))

(ert-deftest ofc-semantic-php-lex-ns-separator ()
  "Lexes a backslash keyword to T_NS_SEPARATOR"
  (should (equal 'T_NS_SEPARATOR (ofc/lex-one-php-token "\\"))))

(ert-deftest ofc-semantic-php-lex-use-declaration ()
  "Lexes a use declaration correctly"
  (should (equal '(T_OPEN_TAG T_USE T_STRING T_SEMICOLON) (ofc/lex-php-tokens "<?php use Foo;"))))

(ert-deftest ofc-semantic-php-lex-use-declaration-with-alias ()
  "Lexes a use declaration correctly"
  (should (equal '(T_OPEN_TAG T_USE T_STRING T_AS T_STRING T_SEMICOLON) (ofc/lex-php-tokens "<?php use Foo as Bar;"))))
