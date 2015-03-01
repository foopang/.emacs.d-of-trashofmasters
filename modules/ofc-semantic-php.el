(require 'semantic)
(require 'semantic/imenu)
(require 'semantic/senator)
(require 'semantic/bovine)

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

;; Apparently this need be fixed because of the following message in
;; Emacs 24.4.5:
;;
;; `semantic-flex' is an obsolete function.  Use `define-lex' to create
;; lexers.
(define-lex-regex-analyzer ofc-semantic-php-lex-prologue
  "Detect and create a prologue token."
  "<[?%]\\(php\\)?\\([[:space:]]+\\|$\\)"
  ;; Zing to the end of this brace block.
  (let ((start (match-beginning 0))
        (end   (match-end 0)))
    (semantic-lex-push-token
     (semantic-lex-token 'PROLOGUE start end))))

(define-lex ofc-semantic-php-lexer
  "Lexical analyzer that handles php buffers.
It ignores whitespaces, newlines and comments."
  semantic-lex-ignore-whitespace
  semantic-lex-ignore-newline
  semantic-lex-ignore-comments
  ofc-semantic-php-lex-prologue

  ;;;; Auto-generated analyzers.
  ofc-semantic-php-wy--<number>-regexp-analyzer
  ofc-semantic-php-wy--<string>-sexp-analyzer

  ;; Must detect keywords before other symbols
  ;; ofc-semantid-php-wy--<keyword>-keyword-analyzer
  ofc-semantic-php-wy--<symbol>-regexp-analyzer
  ofc-semantic-php-wy--<punctuation>-string-analyzer
  ofc-semantic-php-wy--<block>-block-analyzer
  semantic-lex-default-action)

(defun ofc-semantic-php-expand-tag (tag)
  "Expand TAG into a list of equivalent tags, or nil.
Expand multiple variable declarations in the same statement, that is
tags of class `variable' whose name is equal to a list of elements of
the form (NAME START . END).  NAME is a variable name.  START and END
are the bounds in the declaration, related to this variable NAME."
  (let (elts elt clone start end xpand)
    (when (and (eq 'variable (semantic-tag-class tag))
               (consp (setq elts (semantic-tag-name tag))))
      ;; There are multiple names in the same variable declaration.
      (while elts
        ;; For each name element, clone the initial tag and give it
        ;; the name of the element.
        (setq elt   (car elts)
              elts  (cdr elts)
              clone (semantic-tag-clone tag (car elt))
              start (if elts  (cadr elt) (semantic-tag-start tag))
              end   (if xpand (cddr elt) (semantic-tag-end   tag))
              xpand (cons clone xpand))
        ;; Set the bounds of the cloned tag with those of the name
        ;; element.
        (semantic-tag-set-bounds clone start end))
      xpand)))

(defun ofc/semantic-php-setup ()
  "Set up a buffer for semantic parsing of the PHP programming language."
  (ofc-semantic-php-wy--install-parser)

  (setq
   ;; FTTB debug on error
   debug-on-error t

   ;; Lexical analysis
   semantic-lex-number-expression ofc-semantic-php-number-regexp
   semantic-lex-analyzer 'ofc-semantic-php-lexer

   ;; Parsing
   semantic-tag-expand-function 'ofc-semantic-php-expand-tag

   ;; Environment
   semantic-imenu-summary-function 'semantic-format-tag-prototype
   imenu-create-index-function 'semantic-create-imenu-index
   semantic-type-relation-separator-character '(".")
   semantic-command-separation-character ";"
   semantic-lex-comment-regex "\\(/\\*\\|//\\|#\\)"
   ;; speedbar and imenu buckets name
   semantic-symbol->name-assoc-list-for-type-parts
   ;; in type parts
   '((type     . "Classes")
     (variable . "Variables")
     (function . "Methods"))
   semantic-symbol->name-assoc-list
   ;; everywhere
   (append semantic-symbol->name-assoc-list-for-type-parts
           '((include  . "Includes")
             (package  . "Namespaces")))
   ;; navigation inside 'type children
   senator-step-at-tag-classes '(function variable)))

(defun ofc/semantic-php-on ()
  (interactive)
  (ofc/semantic-php-setup))

(provide 'ofc-semantic-php)
