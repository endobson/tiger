#lang racket/base

(require
 (planet endobson/llvm/llvm-simple)
 (planet endobson/llvm/llvm)
 (planet endobson/llvm/llvm-simple-base)) 

(require racket/match racket/list unstable/hash)
(require "lifted-ast.rkt"
         "types.rkt" 
         "primop.rkt"
         "external-functions.rkt")

(provide compile-program write-program)


(define (add-external-functions)


 (let ((i8* (llvm-ptr-type (llvm-int8-type))))
  (llvm-add-function (llvm-fun-type i8* i8* i8* (llvm-int-type)) "memcpy"))

 (define (make-closure funval type name)
  (define closure  (llvm-add-global (closure-type type 0) name))
  (llvm-set-initializer
   closure
   (LLVMConstStructInContext (current-context)
    (list funval (LLVMConstArray (llvm-int-type) empty))
    #f))
  closure)
  

 (for/hash (((name type) external-function-database))
  (case name
   ((exit)

    (define exit-type (convert-function-type type))
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

    (define exit-closure (make-closure exit-function exit-type "exit_closure"))

    (values name exit-closure))
   ((not)
    (define not-type (convert-function-type type))
    (define not-function (llvm-add-function not-type "not"))
    (llvm-set-position (llvm-add-block-to-function not-function))
    (llvm-ret (llvm-zext (llvm-= 0 (llvm-get-param 1)) (llvm-int-type)))
    (values name (make-closure not-function not-type "not_closure")))
   ((getchar)
    (define llvm-type (convert-function-type type))
    (define real-function (llvm-add-function (llvm-fun-type (llvm-int8-type)) "getchar"))
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
    (values name (make-closure function llvm-type "getchar_closure")))
   ((print)
    (define llvm-type (convert-function-type type))
    (define real-function (llvm-add-function
      (llvm-fun-type (llvm-int-type) (llvm-int-type) (llvm-ptr-type (llvm-int8-type)) (llvm-int-type)) "write"))
    (define function (llvm-add-function llvm-type "print"))
    (llvm-set-position (llvm-add-block-to-function function))
    (llvm-call real-function 1 (llvm-bit-cast (llvm-gep (llvm-get-param 1) 0 1) (llvm-ptr-type (llvm-int8-type))) 
                               (llvm-load (llvm-gep (llvm-get-param 1) 0 0)))
    (LLVMBuildRetVoid (current-builder))
    (values name (make-closure function llvm-type "print_closure")))
   ((flush)
    (define llvm-type (convert-function-type type))
    (define real-function (llvm-add-function (llvm-fun-type (llvm-int-type) (llvm-int-type)) "fflush"))
    (define function (llvm-add-function llvm-type "flush"))
    (llvm-set-position (llvm-add-block-to-function function))
    ;(llvm-call real-function 1)
    (LLVMBuildRetVoid (current-builder))
    (values name (make-closure function llvm-type "flush_closure")))
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
    (values name (make-closure function llvm-type "ord_closure")))
   ((chr)
    (define llvm-type (convert-function-type type))
    (define function (llvm-add-function llvm-type "chr"))
    (llvm-set-position (llvm-add-block-to-function function))
    (define int (llvm-get-param 1))
    (define string (llvm-alloc-string 1))
    (llvm-store 1 (llvm-gep string 0 0))
    (llvm-store (llvm-trunc int (llvm-int8-type)) (llvm-gep string 0 1 0))
    (llvm-ret string)
    (values name (make-closure function llvm-type "chr_closure")))
   ((size)
    (define llvm-type (convert-function-type type))
    (define function (llvm-add-function llvm-type "string_size"))
    (llvm-set-position (llvm-add-block-to-function function))
    (llvm-ret (llvm-load (llvm-gep (llvm-get-param 1) 0 0)))
    (values name (make-closure function llvm-type "string_size_closure")))
   ((concat)
    (define llvm-type (convert-function-type type))
    (define function (llvm-add-function llvm-type "string_concat"))
    (define i8* (llvm-ptr-type (llvm-int8-type)))
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
    (values name (make-closure function llvm-type "string_concat_closure")))
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
    (values name (make-closure function llvm-type "substring_closure")))




   (else (values name (error 'code-gen "External-function ~a not yet implemented" name))))))
    
    

  



(define (compile-program prog)
 (define context (LLVMContextCreate))
 (define module (llvm-create-module "program" #:context context))
 (enter-module/32 context module
  (define main-function
    (llvm-add-function
     (llvm-fun-type (llvm-int-type)
      (llvm-int-type) (llvm-ptr-type (llvm-ptr-type (llvm-int8-type))))
     "main"))




  (define ext-functions (add-external-functions))
  (define function-descriptions (lifted-program-functions prog))
  (define all-functions 
   (for/hash (((name fun-desc) function-descriptions))
    (values name
     (llvm-add-function
      (convert-function-type (lifted-function-type fun-desc))
      (symbol->string name)))))

     
  (define info-env (hash-union function-descriptions))
  (define global-environment
   (for/hash (((name primop) runtime-primop-database))
    (values primop (hash-ref ext-functions name))))


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
       (let ((val (compile-expr env info-env all-functions body)))
        (if (unit-type? (function-type-return-type type))
            (LLVMBuildRetVoid (current-builder))
            (llvm-ret val) )))))))



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
         ((unit-type? type) (llvm-void-type))
         ((string-type? type) (llvm-ptr-type (LLVMStructTypeInContext (current-context) (list (llvm-int-type) (LLVMArrayType (llvm-int8-type) 0)) #f)))
         ((box-type? type) (llvm-ptr-type (convert (box-type-elem-type type)))) 
         ((function-type? type)
          (let ((arg-types (map convert (function-type-arg-types type)))
                (return-type (convert (function-type-return-type type))))
            (closure-ptr-type (create-fun-type arg-types return-type) 0)))
         ((record-type? type)
          (let ((field-types (map convert (map cdr (record-type-fields type)))))
            (llvm-ptr-type (LLVMStructTypeInContext (current-context) field-types #f))))
         (else (error 'create-recursive-type "Unsupported type ~a" type)))))
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

(define (llvm-alloc-string size)
 (let ((string (llvm-array-malloc (llvm-int8-type) (llvm+ 4 size))))
  (llvm-bit-cast string (llvm-ptr-type (LLVMStructTypeInContext (current-context) (list (llvm-int-type) (LLVMArrayType (llvm-int8-type) 0)) #f)))))

  
  

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
    ((conditional c t f ty)
     (let ((cv (recur c)))
      (let ((cond (llvm-/= cv 0)))
       (define-basic-block t-block f-block m-block)
       (llvm-cond-br cond t-block f-block)
       (let ((tv (begin (llvm-set-position t-block) (begin0 (recur t) (llvm-br m-block))))
             (fv (begin (llvm-set-position f-block) (begin0 (recur f) (llvm-br m-block)))))
        (llvm-set-position m-block)
        (if (unit-type? ty)
            #f
            (let ((merged (llvm-phi (convert-type ty))))
              (llvm-add-incoming merged (cons tv t-block) (cons fv f-block))
              merged))))))
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
   ((string-constant-primop val) (compile-string-constant val))
   ((nil-primop type) (llvm-null (convert-type type)))
   ((unit-primop) #f)
   ((create-record-primop type) (compile-create-record (convert-type type) vals))
   ((create-box-primop type) (compile-create-box (convert-type type) (first vals)))
   ((create-array-primop type) (compile-create-array
                                 (convert-type (array-type-elem-type type))
                                 (first vals)
                                 (second vals)))
   ((field-ref-primop type name) (compile-field-ref type name (first vals)))
   ((box-ref-primop type) (compile-box-ref type (first vals)))
   ((array-ref-primop type) (compile-array-ref type (first vals) (second vals)))

   ((box-set!-primop type) (compile-box-set! type (first vals) (second vals)))


   ((call-closure-primop ty) (compile-closure-call (first vals) (rest vals)))
   ((runtime-primop type name)
    (hash-ref initial-env op (lambda () (error 'compile-primop "Unknown runtime-primop ~a" op))))
   ((equality-primop equal type) (compile-equality-test equal type (first vals) (second vals)))
   (else (error 'compile-primop "Unsupported primop: ~a" op))))



 (define (compile-string-constant str)
  (define str-length (string-length str))
  (define (llvm-str-type n) (LLVMStructTypeInContext (current-context) (list (llvm-int-type) (LLVMArrayType (llvm-int8-type) n)) #f))
  (define llvm-str (llvm-add-global (llvm-str-type str-length)""))
  (llvm-set-initializer llvm-str
    (LLVMConstStructInContext (current-context) (list (llvm-int str-length) (LLVMConstStringInContext (current-context) str #t)) #f))
  (llvm-bit-cast llvm-str (convert-type string-type)))
  

 (define (compile-equality-test equal type v1 v2)
  (cond
   ((or (int-type? type) (box-type? type) (array-type? type) (record-type? type))
    (llvm-zext ((if equal llvm-= llvm-/=) v1 v2) (llvm-int-type)))
    
   (else (error 'compile-equality-test "Not yet implemented"))))


 (define (compile-create-record type vals)
  (let ((mem (llvm-malloc (llvm-get-element-type type))))
   (for ((v vals) (i (in-naturals)))
    (llvm-store v (llvm-gep mem 0 i)))
   mem))

 (define (compile-field-ref type name val)
  (llvm-load (llvm-gep val 0 (record-type-field-index type name))))


 (define (compile-create-array type size val)
  (let ((mem (llvm-bit-cast (llvm-array-malloc type size) (llvm-ptr-type (LLVMArrayType type 0)))))
   (for ((i (in-range size)))
    (llvm-store val (llvm-gep mem 0 i)))
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
  (llvm-load (llvm-gep array 0 index)))






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


