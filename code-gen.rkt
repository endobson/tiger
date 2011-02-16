#lang racket/base

(require
 (planet endobson/llvm/llvm-simple)
 (planet endobson/llvm/llvm)
 (planet endobson/llvm/llvm-simple-base)) 

(require racket/match racket/list unstable/hash)
(require "lifted-ast.rkt"
         "types.rkt" 
         "primop.rkt")

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

  (define exit-type (create-fun-type (list (llvm-int-type)) (llvm-void-type)))

  (define real-exit-function
   (llvm-add-function
    (llvm-fun-type (llvm-void-type) (llvm-int-type))
    "exit"))
  (define exit-function
   (llvm-add-function
    exit-type
    "exit_clos"))
  
  (llvm-set-position (llvm-add-block-to-function exit-function))
  (llvm-call real-exit-function (llvm-get-param 1))
  (LLVMBuildRetVoid (current-builder))

  (define exit-closure 
   (llvm-add-global
    (closure-type exit-type 0)
    "exit_closure"))
  (llvm-set-initializer
   exit-closure
   (LLVMConstStructInContext (current-context)
    (list 
     exit-function
     (LLVMConstArray (llvm-int-type) empty))
    #f))

  (define function-descriptions (lifted-program-functions prog))
  (define all-functions 
   (for/hash (((name fun-desc) function-descriptions))
    (values name
     (llvm-add-function
      (convert-function-type (lifted-function-type fun-desc))
      (symbol->string name)))))

     
  (define info-env (hash-union function-descriptions))
  (define global-environment
   (hash-union
    (hash (make-runtime-primop (make-function-type (list int-type) unit-type) 'exit) exit-closure)))


  (for (((name fun-desc) function-descriptions))
   (match fun-desc
    ((lifted-function type arg-names closed-names closed-types body)
     (let* ((fun (hash-ref all-functions name))
            (block (llvm-add-block-to-function fun)))
      (llvm-set-position block)
      (let* ((env (for/fold ((env global-environment)) ((arg-name arg-names) (i (in-naturals)))
                  (hash-set env arg-name (llvm-get-param (add1 i)))))
             (env (for/fold ((env env)) ((arg-name closed-names) (arg-type closed-types) (i (in-naturals)))
                   (hash-set env arg-name 
                    (llvm-int-to-ptr
                     (llvm-load (llvm-gep (llvm-get-param 0) 0 1 i))
                     (convert-type arg-type))))))
       (llvm-ret (compile-expr env info-env all-functions body)))))))



  (define main-entry (LLVMAppendBasicBlockInContext (current-context) main-function "entry"))
  (LLVMPositionBuilderAtEnd (current-builder) main-entry)
  (compile-expr global-environment info-env all-functions (lifted-program-expr prog))
  (llvm-ret 0)


  )
 (let ((err (LLVMVerifyModule module 'LLVMReturnStatusAction)))
  (when err
   (LLVMDumpModule module)
   (error 'compile-program "~a" err)))
  

 module)


(define (convert-function-type ty)
 (let ((arg-types (for/list ((t (function-type-arg-types ty))) (convert-type t)))
       (return-type (convert-type (function-type-return-type ty))))
  (create-fun-type arg-types return-type)))

(define (convert-type type)
 (define types (make-hash))
 (define (convert type)
  (hash-ref types type
   (lambda ()
    (let ((opaque (LLVMOpaqueTypeInContext (current-context))))
     (hash-set! types type opaque)
     (let ((rec-type
        (cond
         ((int-type? type) (llvm-int-type))
         ((unit-type? type) (llvm-int-type))
         ((function-type? type)
          (let ((arg-types (map convert (function-type-arg-types type)))
                (return-type (convert (function-type-return-type type))))
            (closure-ptr-type (create-fun-type arg-types return-type) 0)))
         ((record-type? type)
          (let ((field-types (map convert (map (inst cdr Symbol type) (record-type-fields type)))))
            (llvm-ptr (LLVMStructTypeInContext (current-context) field-types))))
         (else (error 'create-recursive-type "Unsupported type ~a")))))
      (let ((handle (LLVMCreateTypeHandle opaque)))
       (LLVMRefineType opaque rec-type)
       (let ((real-type (LLVMResolveTypeHandle handle)))
        (hash-set! types type real-type)
        (LLVMDisposeTypeHandle handle)
        real-type)))))))
 (convert type))
       
    


(define (create-fun-type llvm-arg-types llvm-return-type)
 (let* ((opaque (LLVMOpaqueTypeInContext (current-context)))
        (type-handle (LLVMCreateTypeHandle opaque))) 
  (LLVMRefineType opaque
   (apply llvm-fun-type 
    llvm-return-type
    (llvm-ptr-type             
     (LLVMStructTypeInContext (current-context)
      (list
       (llvm-ptr-type opaque)  
       (LLVMArrayType (llvm-int-type) 0)) #f)) ;closure
    llvm-arg-types))
  (begin0
   (LLVMResolveTypeHandle type-handle)
   (LLVMDisposeTypeHandle type-handle))))


(define (closure-type llvm-fun-type num-free-variables)
 (LLVMStructTypeInContext
  (current-context)
  (list (llvm-ptr-type llvm-fun-type) (LLVMArrayType (llvm-int-type) num-free-variables))
  #f))


(define (closure-ptr-type llvm-fun-type num-free-variables)
 (llvm-ptr-type
  (closure-type llvm-fun-type num-free-variables)))

  
  

(define (int-cast value type env)
 (cond 
  ((int-type? type) value)
  ((unit-type? type) value)
  ((function-type? type) (llvm-ptr-to-int value))
  (else (error 'int-cast "Unsupported-type ~a" type))))



(define (compile-expr initial-env info-env fun-env expr)
 (define (compile env)
  (define (recur expr)
   (match expr
    ((identifier id) (lookup-identifier id env))
    ((bind id ty expr body)
     (let ((expr-value (recur expr)))
      ((compile (hash-set env id expr-value)) body)))
    ((primop-expr op exprs)
     (let ((vals (map recur exprs)))
      (compile-primop op vals)))
    ((sequence first rest)
     (recur first) (recur rest))
    ((conditional c t f)
     (let ((cv (recur c)))
      (let ((cond (llvm-/= cv 0)))
       (define-basic-block t-block f-block m-block)
       (llvm-cond-br cond t-block f-block)
       (let ((tv (begin (llvm-set-position t-block) (begin0 (recur t) (llvm-br m-block))))
             (fv (begin (llvm-set-position f-block) (begin0 (recur f) (llvm-br m-block)))))
        (llvm-set-position m-block)
        (let ((merged (llvm-phi (llvm-int-type))))
         (llvm-add-incoming merged (cons tv t-block) (cons fv f-block))
         merged)))))
    ((bind-rec funs body)
     (define closure-names (map car funs))
     (define fun-names (map (compose create-closure-function cdr) funs))
     (define closed-variables (map (compose create-closure-closed-variables cdr) funs))
     (define num-closed-variables (map length closed-variables))
     (define functions (map (lambda (name) (lookup-function name)) fun-names))
     (define closed-types (map lifted-function-closed-variable-types functions))
     (define llvm-fun-types
      (for/list ((f functions))
       (match f ((lifted-function type arg-names closed-names closed-types body)
         (let ((arg-types (function-type-arg-types type)) (return-type (function-type-return-type type)))
          (create-fun-type (map (lambda (t) (convert-type t)) arg-types)
                           (convert-type return-type)))))))
     (define closures
      (map (lambda (t n) (llvm-malloc (closure-type t n))) llvm-fun-types num-closed-variables))
     (define zero-closures
      (map (lambda (closure t) (llvm-bit-cast closure (llvm-ptr-type (closure-type t 0)))) closures llvm-fun-types))
     (define inner-env (foldl (lambda (name v env) (hash-set env name v)) env closure-names zero-closures))
     (define closed-values
      (map (lambda (vars)
            (map (lambda (id) (lookup-identifier id inner-env))
                 vars))
           closed-variables))
     (for/list ((vals closed-values) (closure closures) (name fun-names) (types closed-types))
       (llvm-store (hash-ref fun-env name) (llvm-gep closure 0 0))
       (for/list ((v vals) (i (in-naturals)) (type types))
        (llvm-store (int-cast v type info-env) (llvm-gep closure 0 1 i))))

     ((compile inner-env) body))
    (else
     (error 'compile "Not yet implemented: ~a" expr))))
  recur)


 (define (compile-primop op vals)
  (match op
   ((math-primop sym) (compile-math sym (first vals) (second vals)))
   ((integer-constant-primop val) val)
   ((nil-primop type) (llvm-null (convert-type type)))
   ((call-closure-primop) (compile-closure-call (first vals) (rest vals)))
   ((runtime-primop type name)
    (hash-ref initial-env op (lambda () (error 'compile-primop "Unknown runtime-primop ~a" op))))
   (else (error 'compile-primop "Unsupported primop: ~a" op))))


 (define (compile-math op l r)
  (define (up-convert x)
   (llvm-zext x (llvm-int-type)))
  (define (down-convert x y)
   (values
    (llvm-/= x 0)
    (llvm-/= y 0)))
  (define ((wrap f) . args)
   (apply f args))

  ((case op
   ((+) llvm+)
   ((-) llvm-)
   ((*) llvm*)
   ((/) llvm/)
   ((=) (compose up-convert llvm-=))
   ((<=) (compose up-convert llvm-<=))
   ((<>) (compose up-convert llvm-/=))
   ((\|) (compose up-convert (wrap llvm-or) down-convert))
   ((&) (compose up-convert (wrap llvm-and) down-convert))
   (else (error 'compile "Math operator ~a not yet implemented" op))) l r))

 (define (compile-closure-call closure args)
   (apply llvm-call
    (llvm-load (llvm-gep closure 0 0))
    closure
    args))

 (define (lookup-identifier id env)
  (hash-ref env id (lambda ()
   (error 'lookup-identifier "Unbound identifier ~a in ~a" id env))))

 (define (lookup-function id)
  (hash-ref info-env id (lambda ()
   (error 'lookup-function "Unknown function ~a" id))))



 ((compile initial-env) expr))


(define (write-program program path)
 (LLVMWriteBitcodeToFile program path))


