#lang typed/racket/base

(require syntax/parse (for-syntax syntax/parse) (for-syntax racket/base) "types.rkt")


(provide external-function-database)

(define-syntax (-> stx) (raise-syntax-error '-> "Used out of context" stx))
(define-syntax (string stx) (raise-syntax-error 'string "Used out of context" stx))
(define-syntax (unit stx) (raise-syntax-error 'unit "Used out of context" stx))
(define-syntax (int stx) (raise-syntax-error 'int "Used out of context" stx))


(define-syntax (external-functions stx)

 (define-syntax-class type 
  #:literals (string unit int)
  (pattern string #:attr ty #'string-type) 
  (pattern int #:attr ty #'int-type) 
  (pattern unit #:attr ty #'unit-type))


 (define-syntax-class fun-dec 
  #:literals (->)
  #:attributes (name (args 1) return)
  (pattern (name:id ((~seq arg:type ... -> ret:type)))
    #:attr (args 1) (attribute arg.ty)
    #:attr return #'ret.ty))

 (syntax-parse stx 
  ((_ (~seq decs:fun-dec ...)) 
   #'(make-immutable-hash
      (list (cons 'decs.name
                  (make-function-type (list decs.args ...) decs.return)) ...)))))

(: external-function-database (HashTable Symbol function-type))
(define external-function-database
 (external-functions
  (itoa (int -> string))
  (atoi (string -> int))
  (readline (-> string))
  (print (string -> unit))
  (flush (-> unit))
  (getchar (-> string))
  (ord  (string -> int))
  (chr (int -> string))
  (size (string -> int))
  (substring (string int int -> string))
  (concat (string string -> string))
  (not (int -> int))
  (exit (int -> unit))))



