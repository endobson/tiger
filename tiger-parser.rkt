#lang racket/base
(require parser-tools/lex
         racket/list
         racket/contract
         parser-tools/yacc
         (prefix-in : parser-tools/lex-sre)
         "core-ast.rkt"
         "source-ast.rkt")



(define-tokens lang-tokens (integer identifier comparison */))
(define-empty-tokens lang-empty-tokens
  (eof for nil break period comma semi-colon colon
   space plus minus and or arrow
   equal of if then else while do to let in end
   type array var function
   assignment open-paren close-paren open-bracket
   close-bracket open-brace close-brace))

(define-lex-abbrev digit (char-set "0123456789"))
(define-lex-abbrev inter-space (:* whitespace))

(define lang-lexer
  (lexer-src-pos
   (whitespace (return-without-pos (lang-lexer input-port)))
   ((:+ digit) (token-integer (string->number lexeme)))
   ("->" (token-arrow))
   ("for" (token-for))
   ("nil" (token-nil))
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
   ("function" (token-function))
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
    (dec ((type id equal ty) (make-type-declaration $2 $4))
         ((var id assignment expr)
          (make-untyped-variable-declaration $2 $4))
         ((var id colon id assignment expr)
          (make-variable-declaration $2 (make-type-reference $4) $6))
         ((function id open-paren tyfields close-paren equal expr)
          (make-function-declaration $2 $4 #f $7))
         ((function id open-paren tyfields close-paren colon id equal expr)
          (make-function-declaration $2 $4 (make-type-reference $7) $9)))
    (ty ((id) (make-type-reference $1))
        ((open-paren ty-seq close-paren arrow ty) (make-function-type $2 $5))
        ((ty arrow ty) (make-function-type (list $1) $3))
        ((open-brace tyfields close-brace) (make-record-type $2))
        ((array of id) (make-array-type $3)))
    
    (tyfields (() empty)
              ((id colon id tyfields-comma) (cons (cons $1 (make-type-reference $3)) $4)))
    (tyfields-comma
     (() empty)
     ((comma id colon id tyfields-comma) (cons (cons $2 (make-type-reference $4)) $5)))
         
    (ty-seq (() empty)
            ((ty ty-seq-comma) (cons $1 $2)))
    (ty-seq-comma
     (() empty)
     ((comma ty ty-seq-comma) (cons $2 $3)))
                  
    
    
    
    
    (expr ((val) $1)
          ((expr +- expr) (prec plus) (make-math $2 $1 $3))
          ((expr */ expr) (make-math $2 $1 $3))
          ((expr and expr) (make-math '& $1 $3))
          ((expr or expr) (make-math '\| $1 $3))
          ((expr comparison expr) (make-math $2 $1 $3))
          ((expr equal expr) (make-math '= $1 $3))

          ((lvalue assignment expr) (make-assignment $1 $3))
          ((expr open-paren close-paren) (make-function-call $1 empty))
          ((expr open-paren expr expr-comma-seq close-paren) 
           (make-function-call $1 (cons $3 $4)))
          ((minus expr) (make-negation $2))
          ((array-creation) $1)
          ((record-creation) $1)
          ((if expr then expr) (make-if-then-else $2 $4 #f))
          ((if expr then expr else expr) (make-if-then-else $2 $4 $6))
          ((while expr do expr) (make-while-loop $2 $4))
          ((for id assignment expr to expr do expr)
           (make-for-loop $2 $4 $6 $8))
          ((let decs in expr-seq end) (make-binder $2 (make-sequence $4)))
          ((open-paren expr-seq close-paren) (make-sequence $2))
          )
    
    (record-creation
     ((id open-brace close-brace)
      (make-create-record (make-type-reference $1) empty))
     ((id open-brace id-equal-expr id-equal-expr-comma-seq close-brace)
      (make-create-record (make-type-reference $1) (cons $3 $4))))
    
    (id-equal-expr ((id equal expr) (cons $1 $3)))
    (id-equal-expr-comma-seq
     (() empty)
     ((comma id-equal-expr id-equal-expr-comma-seq) (cons $2 $3)))
    
     
     
    (array-creation
     ((id open-bracket expr close-bracket of expr)
      (make-create-array $1 $3 $6)))
     
     
    (+- ((plus) '+) ((minus) '-))
    

    
    (expr-comma-seq (() empty)
                    ((comma expr expr-comma-seq) (cons $2 $3)))
    
    (expr-seq (() empty)
              ((expr expr-semi-colon-seq) (cons $1 $2)))
    (expr-semi-colon-seq (() empty)
                         ((semi-colon expr expr-semi-colon-seq) (cons $2 $3)))
    
    (lvalue ((id lvalue2) ($2 (make-identifier $1))))
    
    (lvalue2 (() (lambda (base) base))
             ((open-bracket expr close-bracket lvalue2)
              (lambda (base) ($4 (make-array-ref base $2 #f))))
             ((period id lvalue2)
              (lambda (base) ($3 (make-field-ref base $2 #f)))))
    


    (val ((lvalue) $1)
         ((literal) $1))
    (literal ((integer-literal) $1)
             ((nil) (make-nil #f)))
    (integer-literal ((integer) (make-integer-literal $1)))
    (id ((identifier) $1))
    
    
    )
   (precs 
          
    (nonassoc of open-brace close-brace
              open-bracket close-bracket
              semi-colon 
              close-paren comma if then while do to for let in end)
    (left else)
    (left open-paren)
    (right assignment)
    (left or)
    (left and)
    (nonassoc comparison equal)
    (left plus)
    (left */)
    (nonassoc minus)
    (right arrow)
    )
   (tokens lang-tokens lang-empty-tokens)
   (start expr)
   (src-pos)
   (error
    (lambda (tok-ok? tok-name tok-value start-pos end-pos)
     (if tok-ok?
      (error 'parser "Got unexpected token ~a(~a) at ~a:~a-~a:~a"
       tok-name tok-value
       (position-line start-pos)
       (position-col  start-pos)
       (position-line end-pos)
       (position-col  end-pos))
      (error 'parser "Bad Token at ~a:~a-~a:~a"
       (position-line start-pos)
       (position-col  start-pos)
       (position-line end-pos)
       (position-col  end-pos)))))
      



   (end eof)))

(define (parse p/s)
 (let ((port (if (string? p/s) (open-input-string p/s) p/s)))
  (port-count-lines! port)
  (lang-parser (lambda () (lang-lexer port)))))


(provide/contract
 (parse (-> (or/c string? port?) expression?)))

