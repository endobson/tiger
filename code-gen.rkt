#lang racket/base

(require
 (planet endobson/llvm/llvm-simple)
 (planet endobson/llvm/llvm)) 

(require (for-syntax racket/base))
(require racket/file racket/system racket/match racket/list unstable/hash)
(require "lifted-anf-ast.rkt"
         "types.rkt" 
         "primop.rkt"
         "unique.rkt"
         "external-functions.rkt"
         "code-gen-types.rkt"
         "code-gen-external-functions.rkt")

(provide compile-program write-program optimize-llvm)


(define-syntax (with-temporary-file stx)
 (syntax-case stx ()
  ((_ id body bodies ...)
   #'(let ((id (make-temporary-file)))
      (dynamic-wind
       (let ((first #t)) (lambda () (if first (set! first #f) (error 'run-program "Re-entering protected region"))))
       (lambda () body bodies ...)
       (lambda ()
        (when (file-exists? id)
         (delete-file id))))))))
    

(define (optimize-llvm program)
 (with-temporary-file bitcode
  (with-temporary-file opt-bitcode
   (write-program program bitcode)
   (system* "/usr/bin/env" "opt"
            "-o" (path->string opt-bitcode) 
            "-std-compile-opts"
            (path->string bitcode))
   (let ((buf (LLVMCreateMemoryBufferWithContentsOfFile opt-bitcode)))
     (let ((module (LLVMParseBitcode buf)))
      (LLVMDisposeMemoryBuffer buf)
      module)))))
  

(define (add-helper-functions)
 (llvm-add-function
  (llvm-function-type (llvm-int-type) (llvm-pointer-type (llvm-int8-type)) (llvm-pointer-type (llvm-int8-type)))
  "strcmp"))



(define (compile-program prog)
 (define context (LLVMContextCreate))
 (define module (llvm-create-module "program" #:context context))
 (enter-module/32 context module



  (define stdin-global (llvm-add-global (llvm-pointer-type (llvm-int8-type)) "stdin-file"))
  (llvm-set-initializer stdin-global (llvm-null (llvm-pointer-type (llvm-int8-type))))
  (define-values (ext-functions ext-closures) (add-external-functions stdin-global))
  (define helper-functions (add-helper-functions))

  (define main-function
    (llvm-add-function
     (llvm-function-type (llvm-void-type))
     "tiger_main"))
  (LLVMSetLinkage main-function 'LLVMPrivateLinkage)
  (LLVMSetFunctionCallConv main-function 'LLVMFastCallConv)
  (define real-main-function
    (llvm-add-function
     (llvm-function-type (llvm-int-type)
      (llvm-int-type) (llvm-pointer-type (llvm-pointer-type (llvm-int8-type))))
     "main"))


  (define function-descriptions (lifted-program-functions prog))
  (define all-functions 
   (hash-union
    (for/hash (((name fun-desc) function-descriptions))
     (values name
      (llvm-add-function
       (convert-function-type (function->function-type fun-desc))
       (symbol->string (unique->symbol name)))))
    ext-functions))

     
  (define info-env (hash-union function-descriptions))
  (define global-environment
   (for/hash (((name primop) runtime-primop-database))
    (values primop (hash-ref ext-closures name))))


  (for (((name fun-desc) function-descriptions))
   (match fun-desc
    ((function fun-name type arg-names closed-names closed-types body)
     (let* ((fun (hash-ref all-functions name))
            (block (llvm-add-block-to-function fun)))
      (LLVMSetFunctionCallConv fun 'LLVMFastCallConv)
      (LLVMSetLinkage fun 'LLVMPrivateLinkage)
      (llvm-set-position block)
      (let* ((env (for/fold ((env global-environment)) ((arg-name arg-names) (i (in-naturals)))
                  (hash-set env arg-name (llvm-get-param (add1 i)))))
             (env (for/fold ((env env)) ((arg-name closed-names) (arg-type closed-types) (i (in-naturals)))
                   (hash-set env arg-name 
                    (llvm-int-to-ptr
                     (llvm-load (llvm-gep (llvm-get-param 0) 0 1 i))
                     (convert-type arg-type))))))
        (compile-expr env info-env all-functions body (function-type-return-type type)))))))



  (define main-entry (llvm-add-block-to-function main-function #:name "entry"))
  (llvm-set-position main-entry)
  (compile-expr global-environment info-env all-functions (lifted-program-expr prog) unit-type)

  (define real-main-entry (llvm-add-block-to-function real-main-function #:name "entry"))
  (llvm-set-position real-main-entry)
  (let ((call-inst (llvm-call main-function)))
   (LLVMSetInstructionCallConv call-inst 'LLVMFastCallConv))
  (llvm-ret 0)
  


  )
 (let ((err (llvm-verify-module module)))
  (when err
   (eprintf "~a~n" (llvm-module-description module))
   (error 'compile-program "~a" err)))
  

 module)



       
    





  
  

(define (int-cast value type env)
 (cond 
  ((int-type? type) value)
  ((function-type? type) (llvm-ptr-to-int value))
  ((box-type? type) (llvm-ptr-to-int value))
  ((array-type? type) (llvm-ptr-to-int value))
  ((string-type? type) (llvm-ptr-to-int value))
  (else (error 'int-cast "Unsupported-type ~a" type))))



(define (compile-expr initial-env info-env fun-env expr full-expr-type)
 (define (compile env)
  (define (recur expr)
   (match expr
    ((return id) (llvm-smart-ret (lookup-identifier id env)))
    ((bind-primop var ty op args expr)
     (let ((vals (map (lambda (id) (lookup-identifier id env)) args)))
      ((compile (hash-set env var (compile-primop op vals))) expr)))
    ((conditional c t f ty)
     (let ((cv (lookup-identifier c env)))
      (let ((cond (llvm-/= cv 0)))
       (define-basic-block t-block f-block)
       (llvm-cond-br cond t-block f-block)
       (llvm-set-position t-block)
       (recur t)
       (llvm-set-position f-block)
       (recur f))))
    ((bind-rec funs body)
     (define closure-names (map car funs))
     (define fun-names (map (compose create-closure-function cdr) funs))
     (define closed-variables (map (compose create-closure-closed-variables cdr) funs))
     (define num-closed-variables (map length closed-variables))
     (define functions (map (lambda (name) (lookup-function name)) fun-names))
     (define closed-types (map function-closed-variable-types functions))
     (define llvm-fun-types
      (for/list ((f functions))
       (match f ((function name type arg-names closed-names closed-types body) (convert-function-type type)))))
     (define closures
      (map (lambda (t n) (llvm-malloc (machine-closure-type t n))) llvm-fun-types num-closed-variables))
     (define zero-closures
      (map (lambda (closure t) (llvm-bit-cast closure (llvm-pointer-type (machine-closure-type t)))) closures llvm-fun-types))
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

     ((compile inner-env) body))))
  recur)


 (define (llvm-smart-ret val)
  (if (unit-type? full-expr-type)
      (llvm-ret-void)
      (llvm-ret val)))


 (define (compile-primop op vals)
  (match op
   ((math-primop sym) (compile-math sym (first vals) (second vals)))
   ((integer-constant-primop val) val)
   ((string-constant-primop val) (compile-string-constant val))
   ((nil-primop type) (llvm-null (convert-type type)))
   ((unit-primop) #f)
   ((undefined-primop type) (llvm-get-undef (convert-type type)))
   ((create-record-primop type) (compile-create-record (convert-type type) vals))
   ((create-box-primop type) (compile-create-box (convert-type type) (first vals)))
   ((create-array-primop type) (compile-create-array
                                 (convert-type (array-type-elem-type type))
                                 (first vals)
                                 (second vals)))
   ((field-ref-primop type name) (compile-field-ref type name (first vals)))
   ((box-ref-primop type) (compile-box-ref type (first vals)))
   ((array-ref-primop type) (compile-array-ref type (first vals) (second vals)))
   ((array-set!-primop type) (compile-array-set! type (first vals) (second vals) (third vals)))

   ((box-set!-primop type) (compile-box-set! type (first vals) (second vals)))
   ((field-set!-primop type name) (compile-field-set! type name (first vals) (second vals)))


   ((call-closure-primop ty) (compile-closure-call (first vals) (rest vals)))
   ((call-known-function-primop ty name) (compile-known-function-call name vals))
   ((call-known-runtime-primop ty name) (compile-known-runtime-call name vals))
   ((runtime-primop type name)
    (hash-ref initial-env op (lambda () (error 'compile-primop "Unknown runtime-primop ~a" op))))
   ((equality-primop equal type) (compile-equality-test equal type (first vals) (second vals)))
   (else (error 'compile-primop "Unsupported primop: ~a" op))))



 (define (compile-string-constant str)
  (define str-length (string-length str))
  (define (llvm-str-type n) (llvm-struct-type (llvm-int-type) (llvm-array-type (llvm-int8-type) n)))
  (define llvm-str (llvm-add-global (llvm-str-type (add1 str-length)) ""))
  (llvm-set-initializer llvm-str
    (llvm-struct str-length (string-append str "\0")))
  (llvm-bit-cast llvm-str (convert-type string-type)))
  

 (define (compile-equality-test equal type v1 v2)
  (cond
   ((or (int-type? type) (box-type? type) (array-type? type) (record-type? type))
    (llvm-zext ((if equal llvm-= llvm-/=) v1 v2) (llvm-int-type)))
   ((string-type? type)
    (let ((strcmp (llvm-get-named-function "strcmp")))
     (llvm-zext ((if equal llvm-= llvm-/=) 0 (llvm-call strcmp (llvm-gep v1 0 1 0) (llvm-gep v2 0 1 0))) (llvm-int-type))))
   
    
   (else (error 'compile-equality-test "Not yet implemented for type ~a" type))))


 (define (compile-create-record type vals)
  (let ((mem (llvm-malloc (llvm-get-element-type type))))
   (for ((v vals) (i (in-naturals)))
    (llvm-store v (llvm-gep mem 0 i)))
   mem))

 (define (compile-field-ref type name val)
  (llvm-load (llvm-gep val 0 (record-type-field-index type name))))

 (define (compile-field-set! type name record val)
  (llvm-store val (llvm-gep record 0 (record-type-field-index type name))))


 (define (compile-create-array type size val)
  (let ((mem (llvm-alloc-array type size)))
    (llvm-store size (llvm-gep mem 0 0))
    (for ((i (in-range size)))
         (llvm-store val (llvm-gep mem 0 1 i)))
    mem))

 (define (compile-create-box type val)
   (let ((mem (llvm-malloc (llvm-get-element-type type))))
     (llvm-store val mem)
     mem))

 (define (compile-box-ref type mem)
   (llvm-load mem))

 (define (compile-box-set! type mem val)
   (llvm-store val mem))


 (define (compile-array-ref type array index)
   (llvm-load (llvm-gep array 0 1 index)))

 (define (compile-array-set! type array index val)
   (llvm-store val (llvm-gep array 0 1 index)))






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
      ((>=) (compose up-convert llvm->=))
      ((<) (compose up-convert llvm-<))
      ((>) (compose up-convert llvm->))
      ((<>) (compose up-convert llvm-/=))
      ((\|) (compose up-convert (wrap llvm-or) down-convert))
      ((&) (compose up-convert (wrap llvm-and) down-convert))
      (else (error 'compile "Math operator ~a not yet implemented" op))) l r))

 (define (compile-closure-call closure args)
  (let ((call-inst (llvm-call*
                     (llvm-load (llvm-gep closure 0 0))
                     closure
                     args)))
   (LLVMSetInstructionCallConv call-inst 'LLVMFastCallConv)
   (LLVMSetTailCall call-inst #t)
   call-inst))


 (define (compile-known-function-call function-name args)
  (let ((call-inst  (llvm-call*
                      (hash-ref fun-env function-name)
                      args)))
   (LLVMSetInstructionCallConv call-inst 'LLVMFastCallConv)
   (LLVMSetTailCall call-inst #t)
   call-inst))

 (define (compile-known-runtime-call function-name args)
  (let ((call-inst  (llvm-call*
                      (hash-ref fun-env function-name)
                      args)))
   (LLVMSetInstructionCallConv call-inst 'LLVMFastCallConv)
   (LLVMSetTailCall call-inst #t)
   call-inst))


 (define (lookup-identifier id env)
  (hash-ref env id (lambda ()
   (error 'lookup-identifier "Unbound identifier ~a in ~a" id env))))

 (define (lookup-function id)
  (hash-ref info-env id (lambda ()
   (error 'lookup-function "Unknown function ~a" id))))



 ((compile initial-env) expr))


(define (write-program program path)
 (llvm-write-bitcode-to-file program path))


