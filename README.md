#  Term Rewriting
Simple ARS [Abstract Rewriting System] (https://en.wikipedia.org/wiki/Abstract_rewriting_system) to explore term rewriting strategies and implementation. Heavily inspired by code from @brandonbloom [LispNYC presentation](https://gist.github.com/brandonbloom/ee9bef2b892fddb63f3e), whole presentation [video](https://vimeo.com/155448425)

# Symbolic Algebra 

```
> (algebra '(* (big red circle) (big red circle)))
'(power (big red circle) 2)

> (algebra '(*  (a thing (with something else)) (+ 2 3)))
'(+ (* (a thing (with something else)) 2) (* (a thing (with something else)) 3))

> (algebra '(* (+ 23 (+ 100 73)) (* (+ 2 3) 23)))
'(+ (* (+ 23 100 73) (* 2 3)) (* (+ 23 100 73) (* 2 23)))

```
Evaluating formulas with apply-expr 
```
> (apply-expr (algebra '(* (+ 23 (+ 100 73)) (* (+ 2 3) 23))) '())
10192

```
Providing meaning to symbols where required, like for red cirle and big in (big red cirle)
```
> ((apply-expr (algebra '(*  (big red circle) (+ 2 3))) 
    '([red 'red] [circle 'circle]  [big (lambda (color shape) (if (eq? color 'red) 40 0))]) )
200
```
Perhaps another meaning?

```
> (apply-expr (algebra '(*  (big red circle) (+ 2 3))) '([red 59] [circle 1000]  [big +]))
5295

```

# Symbolic Differentiation

```
> (algebra '(D (* 5 x) x))
5

> (algebra '(D (power x n ) x))
'(* n (power x (- n 1)))

> (algebra '(D  (* 5 (power x 3)) x))
'(* 5 (* 3 (power x (- 3 1))))

> (algebra '(D (+ (power x n ) (* 24 x)) x))
'(+ (* n (power x (- n 1))) 24)

```
