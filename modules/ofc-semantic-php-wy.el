;;; ofc-semantic-php-wy.el --- Generated parser support file

;; Copyright (C) 2015 Andrea Turso

;; Author: Andrea Turso <trashofmasters@gmail.com>
;; Created: 2015-03-04 00:13:25+0000
;; Keywords: syntax
;; X-RCS: $Id$

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of
;; the License, or (at your option) any later version.

;; This software is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; PLEASE DO NOT MANUALLY EDIT THIS FILE!  It is automatically
;; generated from the grammar file ofc-semantic-php.wy.

;;; History:
;;

;;; Code:

(require 'semantic/lex)
(eval-when-compile (require 'semantic/bovine))

;;; Prologue
;;

;;; Declarations
;;
(defconst ofc-semantic-php-wy--keyword-table
  (semantic-lex-make-keyword-table
   '(("(int)" . T_INT_CAST)
     ("(double)" . T_DOUBLE_CAST)
     ("(string)" . T_STRING_CAST)
     ("(array)" . T_ARRAY_CAST)
     ("(object)" . T_OBJECT_CAST)
     ("(bool)" . T_BOOL_CAST)
     ("(unset)" . T_UNSET_CAST)
     ("exit" . T_EXIT)
     ("die" . T_EXIT)
     ("function" . T_FUNCTION)
     ("const" . T_CONST)
     ("return" . T_RETURN)
     ("try" . T_TRY)
     ("catch" . T_CATCH)
     ("throw" . T_THROW)
     ("if" . T_IF)
     ("elseif" . T_ELSEIF)
     ("endif" . T_ENDIF)
     ("else" . T_ELSE)
     ("while" . T_WHILE)
     ("endwhile" . T_ENDWHILE)
     ("do" . T_DO)
     ("for" . T_FOR)
     ("endfor" . T_ENDFOR)
     ("foreach" . T_FOREACH)
     ("endforeach" . T_ENDFOREACH)
     ("declare" . T_DECLARE)
     ("enddeclare" . T_ENDDECLARE)
     ("instanceof" . T_INSTANCEOF)
     ("as" . T_AS)
     ("switch" . T_SWITCH)
     ("endswitch" . T_ENDSWITCH)
     ("case" . T_CASE)
     ("default" . T_DEFAULT)
     ("break" . T_BREAK)
     ("continue" . T_CONTINUE)
     ("echo" . T_ECHO)
     ("print" . T_PRINT)
     ("class" . T_CLASS)
     ("interface" . T_INTERFACE)
     ("extends" . T_EXTENDS)
     ("implements" . T_IMPLEMENTS)
     ("new" . T_NEW)
     ("clone" . T_CLONE)
     ("var" . T_VAR)
     ("eval" . T_EVAL)
     ("include" . T_INCLUDE)
     ("include_once" . T_INCLUDE_ONCE)
     ("require" . T_REQUIRE)
     ("require_once" . T_REQUIRE_ONCE)
     ("global" . T_GLOBAL)
     ("isset" . T_ISSET)
     ("empty" . T_EMPTY)
     ("__halt_compiler" . T_HALT_COMPILER)
     ("static" . T_STATIC)
     ("abstract" . T_ABSTRACT)
     ("final" . T_FINAL)
     ("private" . T_PRIVATE)
     ("protected" . T_PROTECTED)
     ("public" . T_PUBLIC)
     ("unset" . T_UNSET)
     ("list" . T_LIST)
     ("array" . T_ARRAY)
     ("or" . T_LOGICAL_OR)
     ("and" . T_LOGICAL_AND)
     ("xor" . T_LOGICAL_XOR)
     ("__CLASS__" . T_CLASS_C)
     ("__FUNCTION__" . T_FUNC_C)
     ("__METHOD__" . T_METHOD_C)
     ("__LINE__" . T_LINE)
     ("__FILE__" . T_FILE)
     ("namespace (T_NAMESPACE)" . T_NAMESPACE)
     ("__NAMESPACE__ (T_NS_C)" . T_NS_C)
     ("callable (T_CALLABLE)" . T_CALLABLE)
     ("use (T_USE)" . T_USE)
     ("finally (T_FINALLY)" . T_FINALLY)
     ("goto (T_GOTO)" . T_GOTO)
     ("yield (T_YIELD)" . T_YIELD))
   '(("extends" summary "SuperClass|SuperInterfaces declaration: extends <name> [, ...]")
     ("interface" summary "Interface declaration: interface <name>")
     ("class" summary "Class declaration: class <name>")
     ("continue" summary "continue [<label>] ;")
     ("break" summary "break [<label>] ;")
     ("default" summary "switch(<expr>) { ... default: <stmts>}")
     ("case" summary "switch(<expr>) {case <const-expr>: <stmts> ... }")
     ("for" summary "for ([<init-expr>]; [<expr>]; [<update-expr>]) <stmt>")
     ("do" summary "do <stmt> while (<expr>);")
     ("while" summary "while (<expr>) <stmt> | do <stmt> while (<expr>);")
     ("else" summary "if (<expr>) <stmt> else <stmt>")
     ("if" summary "if (<expr>) <stmt> [else <stmt>]")
     ("catch" summary "try {<stmts>} catch(<parm>) {<stmts>} ... ")
     ("try" summary "try {<stmts>} [catch(<parm>) {<stmts>} ...] [finally {<stmts>}]")
     ("return" summary "return [<expr>] ;")
     ("const" summary "Unused reserved word")
     ("implements" summary "Class SuperInterfaces declaration: implements <name> [, ...]")
     ("public" summary "Access level modifier: public {class|interface|<type>} <name> ...")
     ("protected" summary "Access level modifier: protected {class|interface|<type>} <name> ...")
     ("private" summary "Access level modifier: private {class|interface|<type>} <name> ...")
     ("final" summary "Class|Member declaration modifier: final {class|<type>} <name> ...")
     ("abstract" summary "Class|Method declaration modifier: abstract {class|<type>} <name> ...")
     ("static" summary "Declaration modifier: static {class|interface|<type>} <name> ...")
     ("use (T_USE)" summary "Import a class to the current scope: use {class|interface} [as <name>][, ...];")))
  "Table of language keywords.")

(defconst ofc-semantic-php-wy--token-table
  (semantic-lex-make-type-table
   '(("punctuation"
      (T_POW_EQUAL . "**=")
      (T_POW . "**")
      (T_COALESCE . "??")
      (T_ELLIPSIS . "...")
      (T_DOLLAR . "$")
      (T_ASTERISK . "@")
      (T_COMP . "~")
      (T_OR . "|")
      (T_XOR . "^")
      (T_QUESTION . "?")
      (T_URSHIFTEQ . ">>>=")
      (T_URSHIFT . ">>>")
      (T_GT . ">")
      (T_EQUAL . "=")
      (T_LT . "<")
      (T_COLON . ":")
      (T_DIV . "/")
      (T_DOT . ".")
      (T_MINUS . "-")
      (T_COMMA . ",")
      (T_PLUS . "+")
      (T_MULT . "*")
      (T_AND . "&")
      (T_MOD . "%")
      (T_NOT . "!")
      (T_HEREDOC . "<<<")
      (T_DOUBLE_ARROW . "=>")
      (T_SR . ">>")
      (T_SL . "<<")
      (T_BOOLEAN_AND . "&&")
      (T_BOOLEAN_OR . "||")
      (T_XOR_EQUAL . "^=")
      (T_OR_EQUAL . "|=")
      (T_AND_EQUAL . "&=")
      (T_SR_EQUAL . ">>=")
      (T_SL_EQUAL . "<<=")
      (T_MOD_EQUAL . "%=")
      (T_CONCAT_EQUAL . ".=")
      (T_DIV_EQUAL . "/=")
      (T_MUL_EQUAL . "*=")
      (T_MINUS_EQUAL . "-=")
      (T_PLUS_EQUAL . "+=")
      (T_SPACESHIP . "<=>")
      (T_IS_GREATER_OR_EQUAL . ">=")
      (T_IS_SMALLER_OR_EQUAL . "<=")
      (T_IS_NOT_EQUAL . "<>")
      (T_IS_NOT_EQUAL . "!=")
      (T_IS_EQUAL . "==")
      (T_IS_NOT_IDENTICAL . "!==")
      (T_IS_IDENTICAL . "===")
      (T_DEC . "--")
      (T_INC . "++")
      (T_PAAMAYIM_NEKUDOTAYIM . "::")
      (T_SEMICOLON . ";")
      (T_OBJECT_OPERATOR . "->"))
     ("close-paren"
      (BRACK_CLOSE . "]")
      (BRACE_CLOSE . "}")
      (PAREN_CLOSE . ")"))
     ("open-paren"
      (BRACK_OPEN . "[")
      (BRACE_OPEN . "{")
      (PAREN_OPEN . "("))
     ("block"
      (BRACK_BLOCK . "(BRACK_OPEN BRACK_CLOSE)")
      (BRACE_BLOCK . "(BRACE_OPEN BRACE_CLOSE)")
      (PAREN_BLOCK . "(PAREN_OPEN PAREN_CLOSE)"))
     ("code"
      (T_CLOSE_TAG . "?>")
      (T_OPEN_TAG . "<?php")
      (T_NS_SEPARATOR . "\\"))
     ("number"
      (T_NUMBER))
     ("string"
      (T_CONSTANT_ENCAPSED_STRING))
     ("symbol"
      (T_STRING)))
   '(("punctuation" :declared t)
     ("block" :declared t)
     ("code" :declared t)
     ("number" :declared t)
     ("string" :declared t)
     ("symbol" :declared t)
     ("keyword" :declared t)))
  "Table of lexical tokens.")

(defconst ofc-semantic-php-wy--parse-table
  (progn
    (eval-when-compile
      (require 'semantic/wisent/comp))
    (wisent-compile-grammar
     '((T_INT_CAST T_DOUBLE_CAST T_STRING_CAST T_ARRAY_CAST T_OBJECT_CAST T_BOOL_CAST T_UNSET_CAST T_EXIT T_FUNCTION T_CONST T_RETURN T_TRY T_CATCH T_THROW T_IF T_ELSEIF T_ENDIF T_ELSE T_WHILE T_ENDWHILE T_DO T_FOR T_ENDFOR T_FOREACH T_ENDFOREACH T_DECLARE T_ENDDECLARE T_INSTANCEOF T_AS T_SWITCH T_ENDSWITCH T_CASE T_DEFAULT T_BREAK T_CONTINUE T_ECHO T_PRINT T_CLASS T_INTERFACE T_EXTENDS T_IMPLEMENTS T_NEW T_CLONE T_VAR T_EVAL T_INCLUDE T_INCLUDE_ONCE T_REQUIRE T_REQUIRE_ONCE T_GLOBAL T_ISSET T_EMPTY T_HALT_COMPILER T_STATIC T_ABSTRACT T_FINAL T_PRIVATE T_PROTECTED T_PUBLIC T_UNSET T_LIST T_ARRAY T_LOGICAL_OR T_LOGICAL_AND T_LOGICAL_XOR T_CLASS_C T_FUNC_C T_METHOD_C T_LINE T_FILE T_NAMESPACE T_NS_C T_CALLABLE T_USE T_FINALLY T_GOTO T_YIELD T_STRING T_CONSTANT_ENCAPSED_STRING T_NUMBER T_NS_SEPARATOR T_OPEN_TAG T_CLOSE_TAG PAREN_BLOCK BRACE_BLOCK BRACK_BLOCK PAREN_OPEN PAREN_CLOSE BRACE_OPEN BRACE_CLOSE BRACK_OPEN BRACK_CLOSE T_OBJECT_OPERATOR T_SEMICOLON T_PAAMAYIM_NEKUDOTAYIM T_INC T_DEC T_IS_IDENTICAL T_IS_NOT_IDENTICAL T_IS_EQUAL T_IS_NOT_EQUAL T_IS_SMALLER_OR_EQUAL T_IS_GREATER_OR_EQUAL T_SPACESHIP T_PLUS_EQUAL T_MINUS_EQUAL T_MUL_EQUAL T_DIV_EQUAL T_CONCAT_EQUAL T_MOD_EQUAL T_SL_EQUAL T_SR_EQUAL T_AND_EQUAL T_OR_EQUAL T_XOR_EQUAL T_BOOLEAN_OR T_BOOLEAN_AND T_SL T_SR T_DOUBLE_ARROW T_HEREDOC T_NOT T_MOD T_AND T_MULT T_PLUS T_COMMA T_MINUS T_DOT T_DIV T_COLON T_LT T_EQUAL T_GT T_URSHIFT T_URSHIFTEQ T_QUESTION T_XOR T_OR T_COMP T_ASTERISK T_DOLLAR T_ELLIPSIS T_COALESCE T_POW T_POW_EQUAL)
       ((left T_INCLUDE T_INCLUDE_ONCE T_EVAL T_REQUIRE T_REQUIRE_ONCE)
        (left T_COMMA)
        (left T_LOGICAL_OR)
        (left T_LOGICAL_XOR)
        (left T_LOGICAL_AND)
        (right T_PRINT)
        (right T_YIELD)
        (right T_DOUBLE_ARROW)
        (left T_EQUAL T_PLUS_EQUAL T_MINUS_EQUAL T_MUL_EQUAL T_DIV_EQUAL T_CONCAT_EQUAL T_MOD_EQUAL T_AND_EQUAL T_OR_EQUAL T_XOR_EQUAL T_SL_EQUAL T_SR_EQUAL T_POW_EQUAL)
        (left T_QUESTION T_COLON)
        (right T_COALESCE)
        (left T_BOOLEAN_OR)
        (left T_BOOLEAN_AND)
        (left T_OR)
        (left T_XOR)
        (left T_AND)
        (nonassoc T_IS_EQUAL T_IS_NOT_EQUAL T_IS_IDENTICAL T_IS_NOT_IDENTICAL T_SPACESHIP)
        (nonassoc T_LT T_IS_SMALLER_OR_EQUAL 62 T_IS_GREATER_OR_EQUAL)
        (left T_SL T_SR)
        (left T_PLUS T_MINUS T_DOT)
        (left T_MULT T_DIV T_MOD)
        (right T_NOT)
        (nonassoc T_INSTANCEOF)
        (right T_COMP T_INC T_DEC T_INT_CAST T_DOUBLE_CAST T_STRING_CAST T_ARRAY_CAST T_OBJECT_CAST T_BOOL_CAST T_UNSET_CAST T_ASTERISK)
        (right T_POW)
        (right LBRACK)
        (nonassoc T_NEW T_CLONE)
        (left T_ELSEIF)
        (left T_ELSE)
        (left T_ENDIF)
        (right T_STATIC T_ABSTRACT T_FINAL T_PRIVATE T_PROTECTED T_PUBLIC))
       (grammar
        ((T_OPEN_TAG compilation_unit)
         (identity $2)))
       (compilation_unit
        (nil)
        ((use_declaration))
        ((namespace_declaration))
        ((type_declaration)))
       (type_declaration
        ((T_SEMICOLON)
         nil)
        ((class_declaration)))
       (use_declaration
        ((T_USE qualified_name T_AS T_STRING T_SEMICOLON)
         (wisent-raw-tag
          (semantic-tag-new-include $2 nil 'alias $4)))
        ((T_USE qualified_name T_SEMICOLON)
         (wisent-raw-tag
          (semantic-tag-new-include $2 nil)))
        ((T_USE qualified_name_list T_SEMICOLON)))
       (namespace_declaration
        ((T_NAMESPACE qualified_name namespace_body)
         (wisent-raw-tag
          (semantic-tag-new-type $2 $1 $3 nil))))
       (namespace_body
        ((BRACE_BLOCK)
         (semantic-parse-region
          (car $region1)
          (cdr $region1)
          'namespace_member_declaration 1)))
       (namespace_member_declaration
        ((BRACE_OPEN)
         nil)
        ((BRACE_CLOSE)
         nil)
        ((block)
         nil)
        ((use_declaration))
        ((namespace_declaration))
        ((type_declaration)))
       (qualified_name
        ((qualified_name T_NS_SEPARATOR T_STRING)
         (concat $1 "-" $3)))
       (qualified_name_list
        ((qualified_name))
        ((qualified_name_list T_COMMA qualified_name)))
       (require_declaration
        ((T_REQUIRE require_expr T_SEMICOLON)
         (identity $2))
        ((T_REQUIRE_ONCE require_expr T_SEMICOLON)
         (identity $2))
        ((T_INCLUDE require_expr T_SEMICOLON)
         (identity $2))
        ((T_INCLUDE_ONCE require_expr T_SEMICOLON)
         (identity $2)))
       (require_expr
        ((T_CONSTANT_ENCAPSED_STRING)
         (wisent-raw-tag
          (semantic-tag-new-include $1 nil)))
        ((PAREN_BLOCK)
         (wisent-raw-tag
          (semantic-tag-new-include $1 nil))))
       (class_declaration
        ((class_modifiers_opt T_CLASS T_STRING parent_class_opt interfaces_opt class_body)
         (wisent-raw-tag
          (semantic-tag-new-type $3 $2 $6
                                 (if
                                     (or $4 $5)
                                     (cons $4 $5))
                                 :typemodifiers $1))))
       (parent_class_opt
        (nil)
        ((T_EXTENDS qualified_name)
         (identity $2)))
       (interfaces_opt
        (nil)
        ((T_IMPLEMENTS qualified_name_list)
         (nreverse $2)))
       (class_body
        ((BRACE_BLOCK)
         (semantic-parse-region
          (car $region1)
          (cdr $region1)
          'class_member_declaration 1)))
       (class_member_declaration
        (nil))
       (class_modifiers_opt
        (nil)
        ((class_modifiers)
         (nreverse $1)))
       (class_modifiers
        ((class_modifiers class_modifier)
         (cons $2 $1))
        ((class_modifier)
         (list $1)))
       (class_modifier
        ((T_FINAL))
        ((T_ABSTRACT)))
       (block
           ((BRACE_BLOCK))))
     '(grammar compilation_unit require_expr require_declaration type_declaration namespace_declaration class_declaration use_declaration class_body class_member_declaration namespace_member_declaration)))
  "Parser table.")

(defun ofc-semantic-php-wy--install-parser ()
  "Setup the Semantic Parser."
  (semantic-install-function-overrides
   '((parse-stream . wisent-parse-stream)))
  (setq semantic-parser-name "LALR"
        semantic--parse-table ofc-semantic-php-wy--parse-table
        semantic-debug-parser-source #("ofc-semantic-php.wy" 0 19 (help-echo "Mouse-2 toggles maximizing, mouse-3 displays a popup-menu"))
        semantic-flex-keywords-obarray ofc-semantic-php-wy--keyword-table
        semantic-lex-types-obarray ofc-semantic-php-wy--token-table)
  ;; Collect unmatched syntax lexical tokens
  (semantic-make-local-hook 'wisent-discarding-token-functions)
  (add-hook 'wisent-discarding-token-functions
            'wisent-collect-unmatched-syntax nil t))


;;; Analyzers
;;
(define-lex-block-type-analyzer ofc-semantic-php-wy--<block>-block-analyzer
  "block analyzer for <block> tokens."
  "\\s(\\|\\s)"
  '((("(" PAREN_OPEN PAREN_BLOCK)
     ("{" BRACE_OPEN BRACE_BLOCK)
     ("[" BRACK_OPEN BRACK_BLOCK))
    (")" PAREN_CLOSE)
    ("}" BRACE_CLOSE)
    ("]" BRACK_CLOSE))
  )

(define-lex-string-type-analyzer ofc-semantic-php-wy--<punctuation>-string-analyzer
  "string analyzer for <punctuation> tokens."
  "\\(\\s.\\|\\s$\\|\\s'\\)+"
  '((T_POW_EQUAL . "**=")
    (T_POW . "**")
    (T_COALESCE . "??")
    (T_ELLIPSIS . "...")
    (T_DOLLAR . "$")
    (T_ASTERISK . "@")
    (T_COMP . "~")
    (T_OR . "|")
    (T_XOR . "^")
    (T_QUESTION . "?")
    (T_URSHIFTEQ . ">>>=")
    (T_URSHIFT . ">>>")
    (T_GT . ">")
    (T_EQUAL . "=")
    (T_LT . "<")
    (T_COLON . ":")
    (T_DIV . "/")
    (T_DOT . ".")
    (T_MINUS . "-")
    (T_COMMA . ",")
    (T_PLUS . "+")
    (T_MULT . "*")
    (T_AND . "&")
    (T_MOD . "%")
    (T_NOT . "!")
    (T_HEREDOC . "<<<")
    (T_DOUBLE_ARROW . "=>")
    (T_SR . ">>")
    (T_SL . "<<")
    (T_BOOLEAN_AND . "&&")
    (T_BOOLEAN_OR . "||")
    (T_XOR_EQUAL . "^=")
    (T_OR_EQUAL . "|=")
    (T_AND_EQUAL . "&=")
    (T_SR_EQUAL . ">>=")
    (T_SL_EQUAL . "<<=")
    (T_MOD_EQUAL . "%=")
    (T_CONCAT_EQUAL . ".=")
    (T_DIV_EQUAL . "/=")
    (T_MUL_EQUAL . "*=")
    (T_MINUS_EQUAL . "-=")
    (T_PLUS_EQUAL . "+=")
    (T_SPACESHIP . "<=>")
    (T_IS_GREATER_OR_EQUAL . ">=")
    (T_IS_SMALLER_OR_EQUAL . "<=")
    (T_IS_NOT_EQUAL . "<>")
    (T_IS_NOT_EQUAL . "!=")
    (T_IS_EQUAL . "==")
    (T_IS_NOT_IDENTICAL . "!==")
    (T_IS_IDENTICAL . "===")
    (T_DEC . "--")
    (T_INC . "++")
    (T_PAAMAYIM_NEKUDOTAYIM . "::")
    (T_SEMICOLON . ";")
    (T_OBJECT_OPERATOR . "->"))
  'punctuation)

(define-lex-regex-type-analyzer ofc-semantic-php-wy--<symbol>-regexp-analyzer
  "regexp analyzer for <symbol> tokens."
  "\\(\\sw\\|\\s_\\)+"
  nil
  'T_STRING)

(define-lex-regex-type-analyzer ofc-semantic-php-wy--<number>-regexp-analyzer
  "regexp analyzer for <number> tokens."
  semantic-lex-number-expression
  nil
  'T_NUMBER)

(define-lex-sexp-type-analyzer ofc-semantic-php-wy--<string>-sexp-analyzer
  "sexp analyzer for <string> tokens."
  "\\s\""
  'T_CONSTANT_ENCAPSED_STRING)

(define-lex-keyword-type-analyzer ofc-semantic-php-wy--<keyword>-keyword-analyzer
  "keyword analyzer for <keyword> tokens."
  "\\(\\sw\\|\\s_\\)+")


;;; Epilogue
;;

(provide 'ofc-semantic-php-wy)

;; Local Variables:
;; version-control: never
;; no-update-autoloads: t
;; End:

;;; ofc-semantic-php-wy.el ends here
