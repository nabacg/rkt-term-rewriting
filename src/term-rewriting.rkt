#lang typed/racket/no-check
;; borrowed from https://gist.github.com/brandonbloom/ee9bef2b892fddb63f3e inspired by LispNYC https://vimeo.com/155448425
;; simple Term Rewriting with different strategies
;;; Types

(define-type Expr (U Symbol Term))
(define-type Term (Pairof Symbol (Listof Expr)))

(define-type Strategy (-> Expr (Option Expr)))


;;; Conventions

; t is for the subject term
; u, v, w... are for auxilary terms 
; s and p, q, r... are for strategies


;;; Primitives (some of them, anyway)

(define-syntax-rule (rule pattern expr)
  (lambda (t)
    (match t
      [pattern expr]
      [_ #f])))

(define pass (rule x x))

(define fail (rule _ #f))

(define (pipe s p)
  (lambda (t)
    (let ([u (s t)])
      (if u         
          (p u)
          #f))))

(define (alt s . more)
  (lambda (t)
    (let ((u (s t)))
      (cond
        (u u)
        ((null? more) #f)
        (#t ((apply alt more) t))))))

(define term? list?) ; close enough

(define (all s)
  (lambda (t)
    (if (term? t)
      (let ([children (map s (cdr t))])
        (if (for/or ((u children))
              (eq? u #f))
            #f
            (cons (car t) children)))
      t)))


;;; A small subset of a "standard library"

(define (try s)
  (alt s pass))

(define (repeat s)
  (lambda (t)
    ((try (pipe s (repeat s))) t)))

(define (top-down s) ; for comparison, my examples don't use it
  (lambda (t)
    ((pipe s (all (top-down s))) t)))

(define (bottom-up s)
  (lambda (t)
    ((pipe (all (bottom-up s)) s) t)))




(define (innermost s)
  (lambda (t)
    ((bottom-up (try (pipe s (innermost s)))) t)))


;;; Example: Boolean expressions

(define evaluation
  (alt (rule `(not true) 'false)
       (rule `(not false) 'true)
       
       (rule `(and true ,x) x)
       (rule `(and ,x true) x)
       (rule `(and false ,x) 'false)
       (rule `(and ,x false) 'false)
       
       (rule `(or true ,x) 'true)
       (rule `(or ,x true) 'true)
       (rule `(or false ,x) x)
       (rule `(or ,x false) x)
       ))

(define evaluate (bottom-up (repeat evaluation)))

; (evaluate '(not true))
; (evaluate '(and true unknown))
; (evaluate '(and (not false) (or true whatever)))


;;; Example: Conjunctive Normal Form

(define double-negation
  (rule `(not (not ,a)) a))

(define de-morgan
  (alt (rule `(not (and ,a ,b))
             `(or (not ,a) (not ,b)))
       (rule `(not (or ,a ,b))
             `(and (not ,a) (not ,b)))))

(define distribute-or
  (alt (rule `(or (and ,a ,b) ,c)
             `(and (or ,a ,c) (or ,b ,c)))
       (rule `(or ,a (and ,b ,c))
             `(and (or ,a ,b) (or ,a ,c)))))

(define cnf
  (innermost (alt evaluation
                  double-negation
                  de-morgan
                  distribute-or)))

; (cnf '(not (or a b)))
; (cnf '(or (and a b) c))
; (cnf '(and a (or b (and d e))))
; (cnf '(and a (or true (and d e))))

;next: dynamicaly add to those rules

(define derivative-rules
  (alt
   (rule `(D (+ ,f ,g) ,x) `(+ (D ,f ,x) (D ,g ,x)))
   (rule `(D (* ,a (power ,x ,n)) ,x) `(* ,a (D (power ,x ,n) ,x)))
   (rule `(D (power ,x ,n) ,x) `(* ,n (power ,x (- ,n 1))) )
   (rule `(D (* ,a ,x) ,x) `,a)
   (rule `(D (* ,x ,a) ,x) `,a)
   (rule `(D ,a ,x) 0)))

; Derivative
;(algebra '(D (power x n ) x))
;(algebra '(D  (* 5 (power x 3)) x))
;(algebra '(D (+ (power x n ) (* 24 x)) x))
;'(+ (* n (power x (- n 1))) 24)

(define algebra-rules
  (alt
   (rule `(* ,a (+ ,b ,c)) `(+ (* ,a ,b) (* ,a ,c)))
       (rule `(* (+ ,a ,b) ,c) `(+ (* ,a ,c) (* ,b ,c)))
       (rule `(* ,c (+ ,a ,b)) `(+ (* ,a ,c) (* ,b ,c)))
       (rule `(,op ,a) a)
       (rule `(* ,a 1) a)
       (rule `(* ,a 0) 0)
       (rule `(*  0 ,a) 0)
       (rule `(* ,a ,a) `(power ,a 2))
       (rule `(*  1 ,a) a)
       (rule `(/  ,a 1) a)
       (rule `(/  ,a ,a) 1)
       (rule `(/ (power ,a) ,a) a)
       (rule `(+ ,a 0) a)
       (rule `(+  0 ,a) a)
       (rule `(+ ,a (- ,b ,c)) `(- (+ ,a ,b) ,c))
       (rule `(+ ,a (+ ,b ,c)) `(+ ,a ,b ,c))
       (rule `(+ (+ ,a ,b) ,c) `(+ ,a ,b ,c))))

(define algebra
  (innermost (alt derivative-rules algebra-rules)))

;(algebra '(+ 1 (+ 2 3)))
;(algebra '(+ (+ 2 3) 23))
;(algebra '(* (+ 2 3) 23))
;(algebra '(* (+ 23 (+ 100 73)) (* (+ 2 3) 23)))
;(eval (algebra '(* (+ 23 (+ 100 73)) (* (+ 2 3) 23))))
;(algebra '(+ (* 0 x) (* 1 y)))
; (algebra '(* 12 ))

;(algebra '(*  (a thing (with something else)) (+ 2 3)))
; =>
; '(+ (* (a thing (with something else)) 2) (* (a thing (with something else)) 3))


;((algebra '(* (big red circle) (+ 2 3)) )
;'(+ (* (big red circle) 2) (* (big red circle) 3))
;(algebra '(* (big red circle) (big red circle)))
;'(power ,a)

;(algebra '(/ (* (big red circle) (big red circle)) (big red circle)))
; '(big red circle)

; ./ 
(define (apply-expr expr bindings)
  (eval
   `(let ,bindings
      ,expr)))

;(apply-expr (algebra '(*  (big red circle) (+ 2 3))) '([red 'red] [circle 'circle]  [big (lambda (color shape) (if (eq? color 'red) 40 0))]) )
;(apply-expr (algebra '(*  (big red circle) (+ 2 3))) '([red 59] [circle 1000]  [big +]))


(define-syntax in
  (syntax-rules ()
      [(in formula ./ bindings) (apply-expr  (algebra formula) bindings)]))

;(in '(+ 2 2) ./ '([+ (lambda (x y) (+ (* x 100) (* y 10)))]))
;  (in '(+ |2| |2|) ./ '([|2| 42]))
;(in '(*  (big red circle) (+ 2 3)) ./ '([red 59] [circle 1000]  [big +]))
;(in '(*  (big red circle) (+ 2 3)) ./'([red 'red] [circle 'circle]  [big (lambda (color shape) (if (eq? color 'red) 40 0))]) )