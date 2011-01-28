#lang racket
(require parser-tools/lex
         parser-tools/yacc
         (prefix-in : parser-tools/lex-sre))

(define-tokens lang-tokens (integer identifier comparison */))
(define-empty-tokens lang-empty-tokens
  (eof for nil break period comma semi-colon colon
   space plus minus and or
   equal of if then else while do to let in end
   type array var function
   assignment open-paren close-paren open-bracket
   close-bracket open-brace close-brace))

(define-lex-abbrev digit (char-set "0123456789"))
(define-lex-abbrev inter-space (:* whitespace))

(define lang-lexer
  (lexer
   (whitespace (lang-lexer input-port))
   ((:+ digit) (token-integer (string->number lexeme)))
   ("for" (token-for))
   ("nil" (token-for))
   ("of" (token-of))
   ("if" (token-if))
   ("then" (token-then))
   ("else" (token-else))
   ("while" (token-while))
   ("do" (token-do))
   ("to" (token-to))
   ("let" (token-let))
   ("in" (token-in))
   ("end" (token-end))
   ("type" (token-type))
   ("array" (token-array))
   ("var" (token-var))
   ("function" (token-var))
   ("(" (token-open-paren))
   (")" (token-close-paren))
   ("[" (token-open-bracket))
   ("]" (token-close-bracket))
   ("{" (token-open-brace))
   ("}" (token-close-brace))
   ("," (token-comma))
   (";" (token-semi-colon))
   (":" (token-colon))
   
   ("+" (token-plus))
   ("-" (token-minus))
   ("&" (token-and))
   ("|" (token-or))
   ((char-set "*/") (token-*/ (string->symbol lexeme)))
   (":=" (token-assignment))
   ("=" (token-equal))
   ("." (token-period))
   ((:or "<>" "<" "<=" ">" ">=")
    (token-comparison (string->symbol lexeme)))
   ((:: alphabetic (:* alphabetic digit #\")) (token-identifier (string->symbol lexeme)))
   ((eof) (token-eof))))



(define lang-parser
  (parser
   (grammar
    (decs (() empty)
          ((dec decs) (cons $1 $2)))
    (dec ((type id equal ty) (list 'type $2 $4))
         ((var id assignment expr) (list 'var $2 $4 'any))
         ((var id colon id assignment expr) (list 'var $2 $6 $4))
         ((function id open-paren tyfields close-paren equal expr)
          (list 'function $2 $4 $7 'any))
         ((function id open-paren tyfields close-paren colon id equal expr)
          (list 'function $2 $4 $9 $7)))
    (ty ((id) $1)
        ((open-brace tyfields close-brace) (list 'record-type $2))
        ((array of id) (list 'array-type $3)))
    
    (tyfields (() empty)
              ((id colon id tyfields-comma) (cons (list $1 $3) $4)))
    (tyfields-comma
     (() empty)
     ((comma id colon id tyfields-comma) (cons (list $2 $4) $5)))
         
    
    
    
    
    (expr ((val) $1)
          ((expr +- expr) (prec plus) (list $2 $1 $3))
          ((expr */ expr) (list $2 $1 $3))
          ((expr and expr) (list 'and $1 $3))
          ((expr or expr) (list 'or $1 $3))
          ((expr comparison expr) (list $2 $1 $3))
          ((expr equal expr) (list '= $1 $3))

          ((lvalue assignment expr) (list ':= $1 $3))
          ((expr open-paren close-paren) (list 'funcall $1 empty))
          ((expr open-paren expr expr-comma-seq close-paren)
           (list 'funcall $1 (cons $3 $4)))
          ((minus expr) (list '- $2))
          ((array-creation) $1)
          ((record-creation) $1)
          ((if expr then expr) (list 'if $2 $4))
          ((if expr then expr else expr) (list 'if $2 $4 $6))
          ((while expr do expr) (list 'while $2 $4))
          ((for id assignment expr to expr do expr) (list 'for $2 $4 $6 $8))
          ((let decs in expr-seq end) (list 'let $2 $4))
          ((open-paren expr-seq close-paren) (list 'sequence $2))
          )
    
    (record-creation
     ((id open-brace close-brace) (list 'create-record $1 empty))
     ((id open-brace id-equal-expr id-equal-expr-comma-seq close-brace)
      (list 'create-record $1 (cons $3 $4))))
    
    (id-equal-expr ((id equal expr) (list $1 $3)))
    (id-equal-expr-comma-seq
     (() empty)
     ((comma id-equal-expr id-equal-expr-comma-seq) (cons $2 $3)))
    
     
     
    (array-creation
     ((id open-bracket expr close-bracket of expr)
      (list 'create-array $1 $3 $6)))
     
     
    (+- ((plus) '+) ((minus) '-))
    

    
    (expr-comma-seq (() empty)
                    ((comma expr expr-comma-seq) (cons $2 $3)))
    
    (expr-seq (() empty)
              ((expr expr-semi-colon-seq) (cons $1 $2)))
    (expr-semi-colon-seq (() empty)
                         ((semi-colon expr expr-semi-colon-seq) (cons $2 $3)))
    
    (lvalue ((id lvalue2) (list 'lvalue $2 $1)))
    
    (lvalue2 (() empty)
             ((open-bracket expr close-bracket lvalue2) (cons (list 'array-ref $2) $4))
             ((period id lvalue2) (cons (list 'field-ref $2) $3)))
    


    (val ((lvalue) $1)
         ((literal) $1))
    (literal ((integer-literal) $1)
             ((nil) 'nil))
    (integer-literal ((integer) $1))
    (id ((identifier) $1))
    
    
    )
   (precs 
          
    (nonassoc of open-brace close-brace
              open-bracket close-bracket
              semi-colon open-paren
              close-paren comma if then while do to for let in end)
    (left else)
    (right assignment)
    (left or)
    (left and)
    (nonassoc comparison equal)
    (left plus)
    (left */)
    (nonassoc minus))
   (tokens lang-tokens lang-empty-tokens)
   (start expr)
   (error
    (lambda (tok-ok? tok-name tok-value)
      (error 'parser "Got error: ~a ~a ~a" tok-ok? tok-name tok-value)))
   (end eof)))



(define (parse str)
  (let ((port (open-input-string str)))
    (lang-parser (lambda () (lang-lexer port)))))

(parse "1*2+3/4")

;(parse "1=2+3=4")
;(parse "1=2=3=4")
(parse "id:=id.z:=4")

(parse "id:=id[4]:=4")
(parse "--id")
(parse "a & b|c    &d")
(parse "a[x] of 2")
(parse "a{e=3,b=6}")
(parse "if a then if b then c else d")

(parse "let type a = b in a[4] end")