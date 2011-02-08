#lang racket/base

(require
 (planet endobson/llvm/llvm-simple)
 (planet endobson/llvm/llvm)
 (planet endobson/llvm/llvm-simple-base)) 

(require racket/match racket/list)
(require "lifted-ast.rkt" "core-ast.rkt")

(provide compile-program write-program)




(define (compile-program prog)
 (define context (LLVMContextCreate))
 (define module (llvm-create-module "program" #:context context))
 (enter-module/32 context module
  (define main-function
    (llvm-add-function
     (llvm-fun-type (llvm-int-type)
      (llvm-int-type) (llvm-ptr-type (llvm-ptr-type (llvm-int8-type))))
     "main"))
  (define exit-function
   (llvm-add-function
    (llvm-fun-type (llvm-void-type) (llvm-int-type))
    "exit"))

  (define exit-closure 
   (llvm-add-global
    (closure-type* (llvm-fun-type (llvm-void-type) (llvm-int-type)))
    "exit_closure"))
  (llvm-set-initializer
   exit-closure
   (LLVMConstStructInContext (current-context)
    (list 
     exit-function
     (LLVMConstNull (llvm-ptr-type (llvm-int-type))))
    #f))


  (define main-entry (LLVMAppendBasicBlockInContext (current-context) main-function "entry"))
  (LLVMPositionBuilderAtEnd (current-builder) main-entry)
  (compile-expr (hash 'exit exit-closure) (lifted-program-expr prog))
  (llvm-ret 0)


  )
 (let ((err (LLVMVerifyModule module 'LLVMReturnStatusAction)))
  (when err
   (error 'compile-program "~a" err)))
  

 module)


(define (closure-type* fun-type)
 (LLVMStructTypeInContext
  (current-context)
  (list (llvm-ptr-type fun-type) (llvm-ptr-type (llvm-int-type)))
  #f))


(define (closure-type fun-type)
 (llvm-ptr-type
  (closure-type* fun-type)))


(define (compile-expr global-env expr)
 (define (compile env)
  (define (recur expr)
   (match expr
    ((identifier id) (lookup-identifier id env))
    ((integer-literal x) x)
    ((bind id expr body)
     (let ((expr-value (recur expr)))
      ((compile (hash-set env id expr-value)) body)))
    ((string-literal s) (error 'compile "Not Yet Implemented: string-literal"))
    ((negation x) (llvm- 0 (recur x)))
    ((math op left right)
     (let ((l (recur left)) (r (recur right)))
      (compile-math op l r)))
    ((sequence exprs)
     (foldr (lambda (e acc) (recur e)) #f exprs))
    ((function-call function args)
     (let ((f (recur function)) (args (map recur args)))
      (compile-closure-call f args)))
    (else
     (error 'compile "Not yet implemented: ~a" expr))))
  recur)

 (define (compile-math op l r)
  (define (up-convert x)
   (llvm-zext x (llvm-int-type)))
  (define (down-convert x y)
   (values
    (llvm-trunc x (llvm-int1-type))
    (llvm-trunc y (llvm-int1-type))))


  ((case op
   ((+) llvm+)
   ((-) llvm-)
   ((*) llvm*)
   ((/) llvm/)
   ((=) (compose up-convert llvm-=))
   ((<>) (compose up-convert llvm-/=))
   ((\|) (compose up-convert llvm-or down-convert))
   ((&) (compose up-convert llvm-and down-convert))
   (else (error 'compile "Math operator ~a not yet implemented" op))) l r))

 (define (compile-closure-call closure args)
  (apply
   llvm-call
   (llvm-load (llvm-gep closure 0 0))
   args))

 (define (lookup-identifier id env)
  (hash-ref env id (lambda ()
   (hash-ref global-env id (lambda ()
    (error 'lookup-identifier "Unbound identifier ~a"))))))


 ((compile global-env) expr))


(define (write-program program path)
 (LLVMWriteBitcodeToFile program path))


