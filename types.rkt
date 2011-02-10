#lang typed/racket/base


(provide
 type
 proto-type

 type?
 proto-type?

 
 (struct-out proto-function-type)
 (struct-out proto-record-type)
 (struct-out proto-array-type)
 (struct-out proto-box-type)

 unit-type?
 int-type?
 string-type?

 (rename-out
  (unit-type* unit-type)
  (int-type* int-type)
  (string-type* string-type))

 function-type?
 record-type?
 array-type?
 box-type?


 record-type-has-field?
 record-type-field-type
 (rename-out
  (function-type-return-type* function-type-return-type)
  (function-type-arg-types function-type-arg-types)
  (array-type-elem-type* array-type-elem-type)
  (box-type-elem-type* box-type-elem-type)))


(define-type type (U compound-type primitive-type))

(define-type primitive-type
 (U int-type
    unit-type
    string-type))

(define-type compound-type
 (U function-type
    record-type
    array-type
    box-type))

(struct: unit-type () #:transparent)
(struct: int-type () #:transparent)
(struct: string-type () #:transparent)

(define unit-type* (unit-type))
(define int-type* (int-type))
(define string-type* (string-type))






(struct: function-type 
 ((arg-types : (Option (Listof type)))
  (return-type : (Option type))) #:mutable #:transparent)


(struct: record-type 
 ((unique : Symbol) 
  (fields : (Option (Listof (Pair Symbol type))))) #:mutable #:transparent)

(struct: array-type 
 ((unique : Symbol)
  (elem-type : (Option type))) #:mutable #:transparent)

(struct: box-type ((elem-type : (Option type))) #:mutable #:transparent)




(define-type proto-ref-type Symbol)

(define-type proto-type (U proto-compound-type primitive-type)) 
(define-type proto-compound-type
 (U proto-function-type
    proto-record-type
    proto-array-type
    proto-box-type))



(struct: proto-function-type 
 ((arg-types : (Listof proto-ref-type))
  (return-type : proto-ref-type)) #:transparent)


(struct: proto-record-type 
 ((fields : (Listof (Pair Symbol proto-ref-type)))) #:transparent)

(struct: proto-array-type 
 ((unique : Symbol)
  (elem-type : proto-ref-type)) #:transparent)

(struct: proto-box-type ((elem-type : proto-ref-type)) #:transparent)

(define-predicate primitive-type? primitive-type)
(define-predicate type? type)
(define-predicate proto-type? proto-type)


(: fix-proto-types ((Listof (Pair Symbol proto-type)) (HashTable Symbol type) -> (HashTable Symbol type)))


(define (fix-proto-types sym-types env) 

 (: convert-proto (proto-type -> type))
 (define (convert-proto type)
  (cond
   ((proto-function-type? type) (function-type #f #f))
   ((proto-record-type? type)   (record-type (gensym 'id) #f))
   ((proto-array-type? type)    (array-type (gensym 'id) #f))
   ((proto-box-type? type)      (box-type #f))
   (else type)))

 (: fix-type ((HashTable Symbol type) -> (proto-type type -> Void)))
 (define ((fix-type env) proto type)
  (cond
   ((and (proto-function-type? proto)
         (function-type? type))
    (set-function-type-arg-types! type
     (lookup-types (proto-function-type-arg-types proto) env))
    (set-function-type-return-type! type
     (lookup-type (proto-function-type-return-type proto) env)))

   ((and (proto-function-type? proto)
         (function-type? type))
    (set-record-type-fields! type
     (lookup-fields (proto-record-type-fields proto) env)))



   ((and (proto-array-type? proto)
         (array-type? type))
    (set-array-type-elem-type! type
     (lookup-type (proto-array-type-elem-type proto) env)))

   ((and (proto-box-type? proto)
         (box-type? type))
    (set-box-type-elem-type! type
     (lookup-type (proto-box-type-elem-type proto) env)))


   ((and (primitive-type? proto)
         (primitive-type? type)) (void))
   (else (error 'fix-type "Bad pair ~a and ~a" proto type))))



    
    
  

 (: lookup-type (proto-ref-type (HashTable Symbol type)-> type))
 (define (lookup-type sym env)
  (hash-ref env sym
   (lambda () (error 'fix-proto-types "Unbound type name ~a in ~a" sym env))))


 (: lookup-types ((Listof proto-ref-type) (HashTable Symbol type) -> (Listof type)))
 (define (lookup-types syms env)
  (map (lambda: ((s : proto-ref-type)) (lookup-type s env)) syms))


 (: lookup-fields ((Listof (Pair Symbol proto-ref-type)) (HashTable Symbol type) -> (Listof (Pair Symbol type))))
 (define (lookup-fields fields env)
  (let ((names (map (inst car Symbol proto-ref-type) fields))
        (syms  (map (inst cdr Symbol proto-ref-type) fields)))
   (map (inst cons Symbol type)
     names
     (lookup-types syms env))))


 (: add-type (Symbol type (HashTable Symbol type) -> (HashTable Symbol type)))
 (define (add-type sym type env) (hash-set env sym type))

 (let* ((syms (map (inst car Symbol proto-type) sym-types))
        (proto-types (map (inst cdr Symbol proto-type) sym-types))
        (types (map convert-proto proto-types))
        (env (foldl add-type env syms types)))
  (map (fix-type env) proto-types types)
  env))
        





(: record-type-has-field? (record-type Symbol -> Boolean))
(define (record-type-has-field? r-type sym)
 (let ((fields (record-type-fields r-type)))
  (if fields
      (ormap (lambda: ((field : (Pair Symbol type)))
               (equal? sym (car field))) fields)
      (error 'record-type-has-field? "Uninitialized type"))))


(: record-type-field-type (record-type Symbol -> type))
(define (record-type-field-type r-type sym)
 (let ((fields (record-type-fields r-type)))
  (if fields
      (or (ormap (lambda: ((field : (Pair Symbol type)))
            (and (equal? sym (car field)) (cdr field))) fields)
       (error 'record-field-type "Record type ~a has no field of name ~a" r-type sym))
      (error 'record-field-type "Uninitialized type"))))

(: function-type-return-type* (function-type -> type))
(define (function-type-return-type* f-type)
 (or (function-type-return-type f-type)
  (error 'function-type-return-type* "Uninitialized type")))


(: function-type-arg-types* (function-type -> (Listof type)))
(define (function-type-arg-types* f-type)
 (or (function-type-arg-types f-type)
  (error 'function-type-arg-types* "Uninitialized type")))


(: array-type-elem-type* (array-type -> type))
(define (array-type-elem-type* a-type)
 (or (array-type-elem-type a-type)
  (error 'array-type-elem-type "Uninitialized type")))


(: box-type-elem-type* (box-type -> type))
(define (box-type-elem-type* a-type)
 (or (box-type-elem-type a-type)
  (error 'box-type-elem-type "Uninitialized type")))




