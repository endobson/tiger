#lang racket/base
(require "tiger-parser.rkt")
(require "semantic-checks.rkt")

(define (check a) (rename-variables (parse a)))
(check "1*2+3/4")
(check "let var a := 2 in a end")

;(parse "1=2+3=4")
;(parse "1=2=3=4")
;(parse "id:=id.z:=4")

;(parse "id:=id[4]:=4")
;(parse "--id")
;(parse "a & b|c    &d")
;(parse "a[x] of 2")
;(parse "a{e=3,b=6}")
;(parse "if a then if b then c else d")

;(parse "let type a = b in a[4] end")
