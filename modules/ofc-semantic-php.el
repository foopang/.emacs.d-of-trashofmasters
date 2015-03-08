(require 'semantic)
(require 'semantic/wisent)
(require 'ofc-semantic-php-wy)
(require 'ofc-semantic-php-wy-lex)

;;;;
;;;; Semantic integration of the Php LALR parser
;;;;
(defun ofc/semantic-php-setup ()
  "Hook run to setup Semantic in `php-mode'.
Use the alternate LALR(1) parser."
  (ofc-semantic-php-wy--install-parser)
  (setq

   ;; Lexical analysis
   semantic-lex-number-expression ofc-semantic-php-number-regexp
   semantic-lex-analyzer 'ofc-semantic-php-lexer

   ;; Expanding compound tags
   ;; See C-h v semantic-tag-expand-function
   semantic-tag-expand-function 'ofc-semantic-php-expand-tag

   ;; Environment
   imenu-create-index-function 'semantic-create-imenu-index
   semantic-imenu-summary-function 'semantic-format-tag-prototype
   semantic-type-relation-separator-character '(".")
   semantic-command-separation-character ";"
   semantic-lex-comment-regex "\\(/\\*\\|//\\|#\\)"

   ;; speedbar and imenu buckets name
   semantic-symbol->name-assoc-list-for-type-parts '((type     . "Classes")
                                                     (function . "Methods")
                                                     (variable . "Properties"))

   semantic-symbol->name-assoc-list (append
                                     semantic-symbol->name-assoc-list-for-type-parts
                                     '((include  . "Includes")
                                       (package  . "Namespaces")))

   ;; Navigation inside 'type children
   senator-step-at-tag-classes '(function variable))

  ;; Enable semantic mode for the buffer.
  (semantic-mode 1))

(defun ofc-semantic-php-system-include-p (tag)
  "Determines if a semantic tag of class `include' is part of
the vendor, or possibly another, system include folder."
  nil)

(defun ofc-semantic-php-expand-tag (tag)
  "Expand TAG into a list of equivalent tags, or nil.
Expand multiple variable declarations in the same statement, that is
tags of class `variable' whose name is equal to a list of elements of
the form (NAME START . END).  NAME is a variable name.  START and END
are the bounds in the declaration, related to this variable NAME."
  nil)

(add-hook 'php-mode-hook 'ofc/semantic-php-setup)

(provide 'ofc-semantic-php)
