#lang racket/base

(require
 (planet endobson/llvm/llvm-simple))


(require racket/match racket/list)

(require "types.rkt" 
         "primop.rkt"
         "external-functions.rkt"
         "code-gen-types.rkt")

(provide add-external-functions)


(define (add-external-functions)


 (let ((i8* (llvm-pointer-type (llvm-int8-type))))
  (llvm-add-function (llvm-function-type i8* i8* i8* (llvm-int-type)) "memcpy"))

 (define (make-closure funval type name)
  (define closure  (llvm-add-global (machine-closure-type type) name))
  (llvm-set-initializer
   closure
   (llvm-struct funval (llvm-array (llvm-int-type))))
  closure)
  

 (define ext-functions
  (for/hash (((name type) external-function-database))
   (case name
    ((exit)
 
     (define exit-type (convert-function-type type))
     (define real-exit-function
      (llvm-add-function
       (llvm-function-type (llvm-void-type) (llvm-int-type))
       "exit"))
     (define exit-function
      (llvm-add-function
       exit-type
       "exit_clos"))
     
     (llvm-set-position (llvm-add-block-to-function exit-function))
     (llvm-call real-exit-function (llvm-get-param 1))
     (llvm-ret-void)
 
 
     (values name exit-function))
    ((not)
     (define not-type (convert-function-type type))
     (define not-function (llvm-add-function not-type "not"))
     (llvm-set-position (llvm-add-block-to-function not-function))
     (llvm-ret (llvm-zext (llvm-= 0 (llvm-get-param 1)) (llvm-int-type)))
     (values name not-function))
    ((getchar)
     (define llvm-type (convert-function-type type))
     (define real-function (llvm-add-function (llvm-function-type (llvm-int8-type)) "getchar"))
     (define function (llvm-add-function llvm-type "getchar_clos"))
     (llvm-set-position (llvm-add-block-to-function function))
     (define-basic-block error-block succ-block)
     (let ((char (llvm-call real-function)))
      (llvm-cond-br (llvm-= char (llvm-int -1 (llvm-int8-type))) error-block succ-block)
      (llvm-set-position error-block)
      (llvm-ret (llvm-null (convert-type string-type)))
      (llvm-set-position succ-block)
  
      (let ((string (llvm-alloc-string 1)))
       (llvm-store 1 (llvm-gep string 0 0))
       (llvm-store char (llvm-gep string 0 1 0))
       (llvm-ret string)))
     (values name function))
    ((print)
     (define llvm-type (convert-function-type type))
     (define real-function (llvm-add-function
       (llvm-function-type (llvm-int-type) (llvm-int-type) (llvm-pointer-type (llvm-int8-type)) (llvm-int-type)) "write"))
     (define function (llvm-add-function llvm-type "print"))
     (llvm-set-position (llvm-add-block-to-function function))
     (llvm-call real-function 1 (llvm-bit-cast (llvm-gep (llvm-get-param 1) 0 1) (llvm-pointer-type (llvm-int8-type))) 
                                (llvm-load (llvm-gep (llvm-get-param 1) 0 0)))
     (llvm-ret-void)
     (values name function))
    ((flush)
     (define llvm-type (convert-function-type type))
     (define real-function (llvm-add-function (llvm-function-type (llvm-int-type) (llvm-int-type)) "fflush"))
     (define function (llvm-add-function llvm-type "flush"))
     (llvm-set-position (llvm-add-block-to-function function))
     ;(llvm-call real-function 1)
     (llvm-ret-void)
     (values name function))
    ((ord)
     (define llvm-type (convert-function-type type))
     (define function (llvm-add-function llvm-type "ord"))
     (llvm-set-position (llvm-add-block-to-function function))
     (define string (llvm-get-param 1))
     (define-basic-block zero-block non-zero-block)
     (let ((size (llvm-load (llvm-gep string 0 0))))
      (llvm-cond-br (llvm-= 0 size) zero-block non-zero-block)
      (llvm-set-position zero-block)
      (llvm-ret -1)
      (llvm-set-position non-zero-block)
      (llvm-ret (llvm-zext (llvm-load (llvm-gep string 0 1 0)) (llvm-int-type))))
     (values name function))
    ((chr)
     (define llvm-type (convert-function-type type))
     (define function (llvm-add-function llvm-type "chr"))
     (llvm-set-position (llvm-add-block-to-function function))
     (define int (llvm-get-param 1))
     (define string (llvm-alloc-string 1))
     (llvm-store 1 (llvm-gep string 0 0))
     (llvm-store (llvm-trunc int (llvm-int8-type)) (llvm-gep string 0 1 0))
     (llvm-ret string)
     (values name function))
    ((size)
     (define llvm-type (convert-function-type type))
     (define function (llvm-add-function llvm-type "string_size"))
     (llvm-set-position (llvm-add-block-to-function function))
     (llvm-ret (llvm-load (llvm-gep (llvm-get-param 1) 0 0)))
     (values name function))
    ((concat)
     (define llvm-type (convert-function-type type))
     (define function (llvm-add-function llvm-type "string_concat"))
     (define i8* (llvm-pointer-type (llvm-int8-type)))
     (define memcpy (llvm-get-named-function "memcpy"))
     (llvm-set-position (llvm-add-block-to-function function))
     (let ((source-string1 (llvm-get-param 1)) (source-string2 (llvm-get-param 2)))
      (let* ((size1 (llvm-load (llvm-gep source-string1 0 0)))
             (size2 (llvm-load (llvm-gep source-string2 0 0)))
             (size (llvm+ size1 size2))
             (dest-string (llvm-alloc-string size)))
       (llvm-store size (llvm-gep dest-string 0 0))
       (llvm-call memcpy (llvm-bit-cast (llvm-gep dest-string 0 1 0) i8*) (llvm-bit-cast (llvm-gep source-string1 0 1) i8*) size1)
       (llvm-call memcpy (llvm-bit-cast (llvm-gep dest-string 0 1 size1) i8*) (llvm-bit-cast (llvm-gep source-string2 0 1) i8*) size2)
       (llvm-ret dest-string)))
     (values name function))
    ((substring)
     (define llvm-type (convert-function-type type))
     (define function (llvm-add-function llvm-type "substring"))
     (define memcpy (llvm-get-named-function "memcpy"))
     (llvm-set-position (llvm-add-block-to-function function))
     (define-basic-block error-block good-block)
     (let ((source-string (llvm-get-param 1)) (first (llvm-get-param 2)) (n (llvm-get-param 3)))
      (let ((string-size (llvm-load (llvm-gep source-string 0 0))))
       (llvm-cond-br (llvm-or (llvm-> (llvm+ first n) string-size) (llvm->= first string-size)) error-block good-block)
       (llvm-set-position error-block)
       (let ((null-string (llvm-alloc-string 0)))
        (llvm-store 0 (llvm-gep null-string 0 0))
        (llvm-ret null-string))
       (llvm-set-position good-block)
       (let ((dest-string (llvm-alloc-string n)))
        (llvm-store 0 (llvm-gep dest-string 0 0))
        (llvm-call memcpy (llvm-gep dest-string 0 1 0) (llvm-gep source-string 0 1 first) n)
        (llvm-ret dest-string))))
     (values name function))
 
 
 
 
    (else (values name (error 'code-gen "External-function ~a not yet implemented" name))))))
 
 (define ext-closures
  (for/hash (((name function) ext-functions))
   (let ((type (hash-ref external-function-database name)) (closure-name (string-append (symbol->string name) "_closure")))
    (values name (make-closure function (convert-function-type type) closure-name)))))

 (values ext-functions ext-closures))


