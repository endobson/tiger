#lang typed/racket/base

(require "intermediate-ast.rkt" "types.rkt" "primop.rkt" "unique.rkt")
(require racket/match racket/list)
(require racket/pretty)

(provide remove-assignment)

(: hash-union (All (a b) ((HashTable a b) (HashTable a b) * -> (HashTable a b))))
(define (hash-union hash . others)
 (for/fold: : (HashTable a b) 
   ((hash : (HashTable a b) hash))
   ((pair : (Pair a b) ((inst append-map (Pair a b) (HashTable a b)) hash->list others)))
   (hash-set hash (car pair) (cdr pair))))
    


(: remove-assignment (expression -> expression))
(define (remove-assignment prog)
 (define-type set  (HashTable unique type))
 (: find-mutated (expression -> set))
 (define (find-mutated expr)
  (: empty-hash set)
  (define empty-hash (make-immutable-hash empty))
  (: search (set -> (expression -> set)))
  (define (search bound)
   (: recur (expression -> set))
   (define (recur expr )
    (match expr
     ((identifier name) empty-hash)
     ((primop-expr op args) (apply hash-union empty-hash (map recur args)))
     ((conditional c t f ty)
      (hash-union (recur c) (recur t) (recur f)))
     ((bind name ty expr body)
      (hash-union ((search (hash-set bound name ty)) body) (recur expr)))
     ((bind-rec funs body)
      (let-values (((bound env) (fun-recur funs)))
       (hash-union env ((search bound) body))))
     ((while-loop guard body)
      (hash-union (recur guard) (recur body)))
     ((for-loop id init final body)
      (hash-union (recur init) (recur final)
       ((search (hash-set bound id int-type)) body)))
     ((break) empty-hash)
     ((assignment name expr)
      (hash-set (recur expr) name
       (hash-ref bound name
        (lambda () (error 'remove-assignment "Unbound identifier ~a in ~a" name bound)))))
     (else (error 'remove-assignment "Fix has a missing case"))))
   (: fun-recur ((Listof (Pair unique function)) -> (values set set)))
   (define (fun-recur pairs)
    (let* ((syms (map (inst car unique function) pairs))
           (funs (map (inst cdr unique function) pairs))
           (bound (for/fold: : set
                    ((bound : set bound))
                    ((sym : unique syms) (fun : function funs))
                   (hash-set bound sym (function->function-type fun))))
           (hashes (map (lambda: ((fun : function))
                         (match fun
                          ((function args return body)
                           ((search (for/fold: : set
                                      ((bound : set bound))
                                      ((arg : (Pair unique type) args))
                                     (hash-set bound (car arg) (cdr arg)))) body))))
                        funs)))
     (values bound (apply hash-union empty-hash hashes))))

   recur)
  ((search (make-immutable-hash empty)) expr))

 (: mutated set)
 (define mutated (find-mutated prog))

 (: fix (expression -> expression))
 (define (fix expr)
  (match expr
   ((identifier name) 
    (let ((ty (hash-ref mutated name (lambda () #f))))
     (if ty (primop-expr (box-ref-primop (make-box-type ty)) (list expr)) expr)))
   ((primop-expr op args) (primop-expr op (map fix args)))
   ((conditional c t f ty)
    (conditional (fix c) (fix t) (fix f) ty))
   ((bind name ty expr body)
    (if (hash-has-key? mutated name)
        (let ((new-ty (make-box-type ty)))
         (bind name new-ty
          (primop-expr (create-box-primop new-ty) (list (fix expr))) (fix body)))
        (bind name ty (fix expr) (fix body))))
   ((bind-rec functions body)
    (let* ((fun-names (map (inst car unique function) functions))
           (mutated-names (filter (lambda: ((un : unique)) (hash-has-key? mutated un)) fun-names))
           (mutated-new-names (make-immutable-hash (map (inst cons unique unique) mutated-names (map re-uniq mutated-names))))
           (mutation? (not (empty? mutated-names))))
     (if mutation?
         (let* ((new-names (map (lambda: ((name : unique)) (hash-ref mutated-new-names name (lambda () name))) fun-names))
                (new-functions (map (lambda: ((name : unique) (fun : (Pair unique function))) (cons name (cdr fun))) new-names functions)))
          (let ((inside-body 
                 (bind-rec (map fix-function new-functions)
                  (foldl (lambda: ((name : unique) (body : expression))
                           (bind (gen-uniq 'ignore)
                                 unit-type
                                 (primop-expr (box-set!-primop (make-box-type (hash-ref mutated name)))
                                              (list (identifier name) (identifier (hash-ref mutated-new-names name))))
                                 body))
                      (fix body) mutated-names))))
           (foldl (lambda: ((name : unique) (body : expression))
                    (let ((new-type (make-box-type (hash-ref mutated name))))
                      (bind name  new-type (primop-expr (create-box-primop new-type) (list (primop-expr (undefined-primop (hash-ref mutated name)) (list))))
                       body)))
                  inside-body
                  mutated-names)))
         (bind-rec (map fix-function functions) (fix body)))))
   ((while-loop guard body)
    (while-loop (fix guard) (fix body)))
   ((for-loop id init final body)
    (if (hash-has-key? mutated id)
        (error 'remove-assignment "Mutated binding to for loop")
        (for-loop id (fix init) (fix final) (fix body))))
   ((break) expr)
   ((assignment name val)
    (let ((ty (hash-ref mutated name)))
     (primop-expr (box-set!-primop (make-box-type ty)) (list (identifier name) (fix val)))))
   (else (error 'remove-assignment "Fix has a missing case"))))

 (: fix-function ((Pair unique function) -> (Pair unique function)))
 (define (fix-function pair)
  (let ((fun (cdr pair)))
   (cons (car pair) 
    (match fun
     ((function args ty body)
      (let* ((arg-names (map (inst car unique type) args))
             (arg-types (map (inst cdr unique type) args))
             (mut-args (map (lambda: ((sym : unique))
                             (and (hash-has-key? mutated sym)
                                  (re-uniq sym))) arg-names))
             (new-arg-names (map (lambda: ((new : (Option unique)) (old : unique))
                                  (or new old)) mut-args arg-names)))
       (function (map (inst cons unique type) new-arg-names arg-types) ty
        (for/fold: : expression
          ((expr : expression (fix body)))
          ((new : (Option unique) mut-args)
           (old : unique arg-names)
           (ty : type arg-types))
         (if new
          (let ((new-ty (make-box-type ty)))
           (bind old new-ty
            (primop-expr (create-box-primop new-ty) (list (identifier new)))
            expr))
          expr)))))))))


      
      

  
 (fix prog))
