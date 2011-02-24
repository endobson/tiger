#lang racket/base


(require
 (planet endobson/llvm/llvm-simple)
 (planet endobson/llvm/llvm)) 

(require racket/list)
(require  "types.rkt" )

(provide convert-type machine-closure-type convert-function-type llvm-alloc-string llvm-alloc-array)

(define (machine-string-type (size 0))
 (llvm-pointer-type (llvm-struct-type (llvm-int-type) (llvm-array-type (llvm-int8-type) size))))

(define (machine-array-type type (size 0))
 (llvm-pointer-type (llvm-struct-type (llvm-int-type) (llvm-array-type type size))))


(define (machine-function-type llvm-arg-types llvm-return-type)
 (let* ((opaque (llvm-opaque-type))
        (type-handle (LLVMCreateTypeHandle opaque))) 
  (LLVMRefineType opaque
   (llvm-function-type*
    llvm-return-type
    (llvm-pointer-type             
     (llvm-struct-type
       (llvm-pointer-type opaque)  
       (llvm-array-type (llvm-int-type)))) ;closure
    llvm-arg-types))
  (begin0
   (LLVMResolveTypeHandle type-handle)
   (LLVMDisposeTypeHandle type-handle))))


(define (machine-closure-type llvm-fun-type (num-free-variables 0))
 (llvm-struct-type
  (llvm-pointer-type llvm-fun-type)
  (llvm-array-type (llvm-int-type) num-free-variables)))



(define (convert-type type)
 (define types (make-hash))
 (define (convert type)
  (hash-ref types type
   (lambda ()
    (let ((opaque (llvm-opaque-type)))
     (hash-set! types type opaque)
     (let ((rec-type
        (cond
         ((int-type? type) (llvm-int-type))
         ((unit-type? type) (llvm-void-type))
         ((string-type? type) (machine-string-type))
         ((box-type? type) (llvm-pointer-type (convert (box-type-elem-type type)))) 
         ((array-type? type) (machine-array-type (convert (array-type-elem-type type))))
         ((function-type? type)
          (let ((arg-types (map convert (function-type-arg-types type)))
                (return-type (convert (function-type-return-type type))))
            (llvm-pointer-type (machine-closure-type (machine-function-type arg-types return-type) 0))))
         ((record-type? type)
          (let ((field-types (map convert (map cdr (record-type-fields type)))))
            (llvm-pointer-type (llvm-struct-type* field-types))))
         (else (error 'create-recursive-type "Unsupported type ~a" type)))))
      (let ((handle (LLVMCreateTypeHandle opaque)))
       (LLVMRefineType opaque rec-type)
       (let ((real-type (LLVMResolveTypeHandle handle)))
        (hash-set! types type real-type)
        (LLVMDisposeTypeHandle handle)
        real-type)))))))
 (convert type))




(define (convert-function-type ty)
 (let ((arg-types (for/list ((t (function-type-arg-types ty))) (convert-type t)))
       (return-type (convert-type (function-type-return-type ty))))
  (machine-function-type arg-types return-type)))


(define (llvm-alloc-string size)
 (let ((string (llvm-array-malloc (llvm-int8-type) (llvm+ 4 size))))
  (llvm-bit-cast string (machine-string-type))))


(define (llvm-alloc-array type size)
 (let ((array (llvm-array-malloc type (llvm+ 1 size))))
  (llvm-bit-cast array (machine-array-type type))))

