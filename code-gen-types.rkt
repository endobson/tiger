#lang racket/base


(require
 (planet endobson/llvm/simple))

(require racket/list)
(require  "types.rkt" )

(provide convert-type machine-closure-type convert-function-type llvm-alloc-string llvm-alloc-array)

(define (machine-string-type (size 0))
 (llvm-pointer-type (llvm-struct-type (llvm-int-type) (llvm-array-type (llvm-int8-type) size))))

(define (machine-array-type type (size 0))
 (llvm-pointer-type (llvm-struct-type (llvm-int-type) (llvm-array-type type size))))

(define machine-function-type-hash (make-hash))
(define machine-closure-type-hash (make-hash))


(define (machine-function-type llvm-arg-types llvm-return-type)
  (define key (cons llvm-return-type llvm-arg-types))
  (hash-ref! machine-function-type-hash key
   (lambda ()
    (define named-struct (llvm-named-struct-type "function_args"))
    (define function-type
      (llvm-function-type*
        llvm-return-type
        (llvm-pointer-type
          named-struct)
        llvm-arg-types))
    (llvm-struct-set-body! named-struct
      (llvm-pointer-type function-type)
      (llvm-array-type (llvm-int-type)))
    (hash-set! machine-closure-type-hash function-type named-struct)
    function-type)))


(define (machine-closure-type llvm-fun-type (num-free-variables 0))
  (if (zero? num-free-variables)
      (hash-ref machine-closure-type-hash llvm-fun-type)
      (llvm-struct-type
        (llvm-pointer-type llvm-fun-type)
        (llvm-array-type (llvm-int-type) num-free-variables))))



(define types (make-hash))
(define (convert-type type)
 (define (convert type)
  (hash-ref types type
   (lambda ()
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
      (let ((named-struct (llvm-named-struct-type)))
        (hash-set! types type (llvm-pointer-type named-struct))
        (let ((field-types (map convert (map cdr (record-type-fields type)))))
          (llvm-struct-set-body*! named-struct field-types))
        (llvm-pointer-type named-struct)))
     (else (error 'create-recursive-type "Unsupported type ~a" type))))))
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

