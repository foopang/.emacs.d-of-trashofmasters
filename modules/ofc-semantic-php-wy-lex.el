(defconst ofc-semantic-php-number-regexp
  (eval-when-compile
    (concat "\\("
            "\\<[0-9]+[.][0-9]+\\([eE][-+]?[0-9]+\\)?[fFdD]?\\>"
            "\\|"
            "\\<[0-9]+[.][eE][-+]?[0-9]+[fFdD]?\\>"
            "\\|"
            "\\<[0-9]+[.][fFdD]\\>"
            "\\|"
            "\\<[0-9]+[.]"
            "\\|"
            "[.][0-9]+\\([eE][-+]?[0-9]+\\)?[fFdD]?\\>"
            "\\|"
            "\\<[0-9]+[eE][-+]?[0-9]+[fFdD]?\\>"
            "\\|"
            "\\<0[xX][0-9a-fA-F]+[lL]?\\>"
            "\\|"
            "\\<[0-9]+[lLfFdD]?\\>"
            "\\)"
            ))
  "Lexer regexp to match Java number terminals.
Following is the specification of Java number literals.

DECIMAL_LITERAL:
    [1-9][0-9]*
  ;
HEX_LITERAL:
    0[xX][0-9a-fA-F]+
  ;
OCTAL_LITERAL:
    0[0-7]*
  ;
INTEGER_LITERAL:
    <DECIMAL_LITERAL>[lL]?
  | <HEX_LITERAL>[lL]?
  | <OCTAL_LITERAL>[lL]?
  ;
EXPONENT:
    [eE][+-]?[09]+
  ;
FLOATING_POINT_LITERAL:
    [0-9]+[.][0-9]*<EXPONENT>?[fFdD]?
  | [.][0-9]+<EXPONENT>?[fFdD]?
  | [0-9]+<EXPONENT>[fFdD]?
  | [0-9]+<EXPONENT>?[fFdD]
  ;")

(defconst ofc-semantic-php-label-regex
  "[a-zA-Z_\x7f-\xff][a-zA-Z0-9_\x7f-\xff]*")

(defun ofc-semantic-php--move-to-php-beginning ()
  (if (re-search-forward "<[%?]" nil t)
      (cond
       ((or (looking-at "\\(php\\)?$")
	    (looking-at "\\(php\\)?[[:space:]])"))
	(goto-char (match-end 0))
	'T_NONPHP)
       ((or (looking-at "=$")
	    (looking-at "=[[:space:]]"))
	'T_ECHO_BLOCK)
       (t
	(ofc-semantic-php--move-to-php-beginning)))
    (goto-char (point-max))
    nil))

(define-lex-regex-analyzer ofc-semantic-php-lex-ns-separator
  "Replace backslashes with a T_NS_SEPARATOR."
  "\\\\"
  (let ((start (point))
        (end   (match-end 0)))
    (message (format "Found namespace separator from %d until %d" start end))
    (semantic-lex-push-token
     (semantic-lex-token 'T_NS_SEPARATOR start end))))

(define-lex-regex-analyzer ofc-semantic-php-lex-prologue
  "Detects and converts a php opening tag to a T_OPEN_TAG token"
  "<[?%]\\(php\\)?\\([[:space:]]+\\|$\\)"
  ;; Zing to the end of this brace block.
  (let ((start (match-beginning 0))
        (end   (match-end 0)))
    (semantic-lex-push-token
     (semantic-lex-token 'T_OPEN_TAG start end))))

(define-lex-regex-analyzer ofc-semantic-php-lex-epilogue
  "Detects and converts a php closing tag to a T_CLOSE_TAG token"
  "[%?]>"
  (let ((start (match-beginning 0))
        (end   (match-end 0)))
    (semantic-lex-push-token
     (semantic-lex-token 'T_CLOSE_TAG start end))))

;; Define the lexer for this grammar
(define-lex ofc-semantic-php-lexer
  "Lexical analyzer that handles php buffers.
It ignores whitespaces, newlines and comments."
  ;; Ignore whitespace, newline and comments.
  semantic-lex-ignore-whitespace
  semantic-lex-ignore-newline

  ;; Must detect prologue/epilogue before other symbols/keywords.
  ofc-semantic-php-lex-prologue
  ofc-semantic-php-lex-epilogue

  ;; Must detect qualified names separately.
  ofc-semantic-php-lex-ns-separator

  ;; Auto-generated analyzers.
  ofc-semantic-php-wy--<number>-regexp-analyzer
  ofc-semantic-php-wy--<string>-sexp-analyzer

  ;; Must detect comments after strings because `comment-start-skip'
  ;; regexp match semicolons inside strings!
  semantic-lex-ignore-comments

  ;; Must detect keywords before other symbols
  ofc-semantic-php-wy--<keyword>-keyword-analyzer

  ofc-semantic-php-wy--<symbol>-regexp-analyzer
  ofc-semantic-php-wy--<punctuation>-string-analyzer
  ofc-semantic-php-wy--<block>-block-analyzer
  semantic-lex-default-action)

;; Taken from Emacs Cedet sources (semantic-grammar.el)
(defun ofc/semantic-php-lex-buffer ()
  "Run `ofc-semantic-lex' on current buffer."
  (interactive)
  (semantic-lex-init)
  (setq semantic-lex-analyzer 'ofc-semantic-php-lexer)
  (let ((token-stream
         (semantic-lex (point-min) (point-max))))
    (with-current-buffer (get-buffer-create "*ofc-semantic-php-lex*")
      (erase-buffer)
      (pp token-stream (current-buffer))
      (goto-char (point-min))
      (pop-to-buffer (current-buffer)))))

(provide 'ofc-semantic-php-wy-lex)

;;; ofc-semantic-wy-lex.el ends here.
