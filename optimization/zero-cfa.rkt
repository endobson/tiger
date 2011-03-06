#lang typed/racket/base

(require "../primop.rkt" "../ir-anf-ast.rkt" "../types.rkt")
(require racket/list racket/match)

(provide zero-cfa)

(define-type (Set a) (HashTable a #t))

(: zero-cfa (expression -> (HashTable unique (Set unique))))
(define (zero-cfa expr)

 (: empty-set (Set unique))
 (define empty-set (make-immutable-hash empty))
 
 (: set-add  (All (a) ((Set a) a -> (Set a))))
 (define (set-add set a)
  (hash-set set a #t))

 (: set-union  (All (a) ((Set a) (Set a) -> (Set a))))
 (define (set-union set add)
  (for/fold: : (Set a)
   ((set : (Set a) set))
   ((elem : a (hash-map add (lambda: ((k : a) (v : #t)) k))))
   (set-add set elem)))

 (: set-member?  (All (a) ((Set a) a -> Boolean)))
 (define (set-member? set a)
  (hash-has-key? set a))
 
 (: set-size (All (a) ((Set a) -> Natural)))
 (define (set-size set)
  (hash-count set))
 
 (: current-env-time-stamp Natural)
 (define current-env-time-stamp 0)
 (: current-store-time-stamp Natural)
 (define current-store-time-stamp 0)


 (: environment (HashTable unique (Set unique)))
 ;Variable Names to possible function names
 ;function names to possible function names that they return
 (define environment (make-hash))

 (: lookup-variable (unique -> (Set unique)))
 (define (lookup-variable var)
  (hash-ref environment var (lambda () empty-set)))

 (: update-environment! (unique (Set unique) -> Void))
 (define (update-environment! var set)
  (hash-set! environment var set)
  (set! current-env-time-stamp (add1 current-env-time-stamp)))


 (: maybe-update-environment! (unique (Set unique) -> Void))
 (define (maybe-update-environment! var set)
  (let ((old-set (lookup-variable var)))
   (when (> (set-size set) (set-size old-set))
    (update-environment! var set))))


 (: store (HashTable type (Set unique))) ;Types to possible function names
 (define store (make-hash))

 (: memo-table (HashTable unique (Pair Natural Natural)))
 ;Variable Names + function names -> time stamps
 ;Last time that variable was called or function was called
 ;Currently No functions memoized
 (define memo-table (make-hash))

 (: memoise (unique -> Void))
 (define (memoise sym)
  (hash-set! memo-table sym (cons current-env-time-stamp current-store-time-stamp)))
 


 (: functions (HashTable unique function))
 (define functions
  (let ()
   (: find-functions (expression -> (Listof function)))
   (define (find-functions expr)
    (match expr
     ((return name) empty)
     ((bind-primop var ty op args expr)
      (find-functions expr))
     ((bind-rec funs body)
      (apply append
       (map (inst cdr unique function) funs)
       (find-functions body)
       (map (lambda: ((p : (Pair unique function)))
        (find-functions (function-body (cdr p)))) funs)))
     ((conditional c t f ty)
      (append (find-functions t) (find-functions f)))))
     
 
     (make-immutable-hash
       (map (lambda: ((f : function)) (cons (function-name f) f)) (find-functions expr)))))


 (: call (unique unique (Listof unique) -> (Set unique)))
 (define (call label fun-sym args)
  (let ((old-time-stamps (hash-ref memo-table label (lambda () (cons -1 -1)))))
   (if (or (<  (car old-time-stamps) current-env-time-stamp)
           (<  (cdr old-time-stamps) current-store-time-stamp))
    (let ((fun-names (lookup-variable fun-sym))
          (original-funs (lookup-variable label)))
     (memoise label)
     (let ((new-funs 
            (for/fold: : (Set unique)
             ((old-funs : (Set unique) original-funs))
             ((fun : unique (hash-map fun-names (lambda: ((k : unique) (v : #t)) k))))
             (set-union old-funs (application fun (map lookup-variable args))))))
      (when (> (set-size new-funs) (set-size original-funs))
       (update-environment! label new-funs))
      new-funs))
    (lookup-variable label))))

 (: application (unique (Listof (Set unique)) -> (Set unique)))
 (define (application fun-name arg-values)
  (let ((fun (hash-ref functions fun-name)))
   (match fun
    ((function name typed-args ret-type body)
     (let ((original-time-stamp current-env-time-stamp))
      (for: ((pair : (Pair unique type) typed-args)
             (arg-values : (Set unique) arg-values))
       (let* ((sym (car pair))
              (old-values (lookup-variable sym))
              (new-values (set-union old-values arg-values)))
        (when (> (set-size new-values) (set-size old-values))
         (update-environment! sym new-values))))
      (let ((new-time-stamp current-env-time-stamp))
       (define (calculate-return)
        (let ((new-values (process body)))
         (update-environment! fun-name new-values)
         new-values))

       (if (<= new-time-stamp original-time-stamp)
           (hash-ref environment fun-name calculate-return)
           (calculate-return))))))))


 (: process (expression -> (Set unique)))
 (define (process expr)
  (match expr
   ((return name) (lookup-variable name))
   ((bind-rec funs body)
    (for: ((p : (Pair unique function) funs))
     (maybe-update-environment! (car p) (set-add empty-set (function-name (cdr p)))))
    (process body))
   ((conditional c t f ty)
    (set-union (process t) (process f)))
   ((bind-primop var ty op args expr)
    (define (do-call) (call var (first args) (rest args)))
    (match op
     ((call-closure-primop ty) (do-call))
     ((call-known-function-primop ty name) (do-call))
     (else (void)))
    (process expr))))


 (process expr)
 environment)
