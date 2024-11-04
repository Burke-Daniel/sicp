#lang sicp
(define (memq item x)
  (cond
    ((null? x) false)
    ((eq? item (car x)) x)
    (else (memq item (cdr x)))))

; Exercise 2.53
(list 'a 'b 'c)
; (a b c)

(list (list 'george))
; ((george))

(cdr '((x1 x2) (y1 y2)))
; ((y1 y2))

(cadr '((x1 x2) (y1 y2)))
; (y1 y2)

(pair? (car '(a short list)))
; #f

(memq 'red '((red shoes) (blue socks)))
; #f

(memq 'red '(red shoes blue socks))
; (red shoes blue socks)

; Exercise 2.54
(define (equal? a b)
  (cond
    ((and (pair? a) (pair? b))
     (and (eq? (car a) (car b)) (equal? (cdr a) (cdr b))))
    ((and (not (pair? a)) (not (pair? b)))
     (eq? a b))
    (else #f)))

; ---------------------------------

; Exercise 2.55
(car (quote (quote (abracadabra))))

; ---------------------------------


; Assume we have the following procedures:
#| (variable? e) Is e a variable? |#
#| (same-variable? v1 v2) Are v1 and v2 the same variable? |#
#| (sum? e) Is e a sum? |#
#| (addend e) Addend of the sum e. |#
#| (augend e) Augend of the sum e. |#
#| (make-sum a1 a2) Construct the sum of a1 and a2. |#
#| (product? e) Is e a product? |#
#| (multiplier e) Multiplier of the product e. |#
#| (multiplicand e) Multiplicand of the product e. |#
#| (make-product m1 m2) Construct the product of m1 and m2 |#

(define (deriv exp var)
  (cond
    ((number? exp) 0)
    ((variable? exp) (if (same-variable? exp var) 1 0))
    ((sum? exp) (make-sum (deriv (addend exp) var)
                          (deriv (augend exp) var)))
    ((product? exp) (make-sum
                     (make-product (multiplier exp)
                                   (deriv (multiplicand exp) var))
                     (make-product (deriv (multiplier exp) var)
                                   (multiplicand exp))))
    ((exponentiation? exp) (make-product
                            (make-product
                             (exponent exp)
                             (make-exponentiation (base exp) (make-sum (exponent exp) -1)))
                            (deriv (base exp) var)))
    (else
     (error "unknown expression type: DERIV" exp))))
                                   
(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

#| (define (make-sum a1 a2) (list '+ a1 a2)) |#
; New definition that will simplify expression if both are numbers
(define (=number? exp num) (and (number? exp) (= exp num)))
(define (make-sum a1 a2)
  (cond
    ((=number? a1 0) a2)
    ((=number? a2 0) a1)
    ((and (number? a1) (number? a2))
     (+ a1 a2))
    (else (list '+ a1 a2))))
#| (define (make-product m1 m2) (list '* m1 m2)) |#
; New definition so that products of numbers are evaluated
(define (make-product m1 m2)
  (cond
    ((or (=number? m1 0) (=number? m2 0)) 0)
    ((=number? m1 1) m2)
    ((=number? m2 1) m1)
    ((and (number? m1) (number? m2) (* m1 m2)))
    (else (list '* m1 m2))))
(define (multiplier p) (cadr p))
(define (multiplicand p) (caddr p))
(define (sum? x) (and (pair? x) (eq? (car x) '+)))
(define (addend s) (cadr s))
(define (augend s) (caddr s))
(define (product? x) (and (pair? x) (eq? (car x) '*)))
(define (exponentiation? x) (and (pair? x) (eq? (car x) '**)))
(define (base x) (cadr x))
(define (exponent x) (caddr x))
(define (make-exponentiation base exponent)
  (cond
    ((=number? base 0) 0)
    ((=number? exponent 0) 1)
    ((=number? exponent 1) base)
    ((and (number? base) (number? exponent)) (expt base exponent))
    (else (list '** base exponent))))

; Exercise 2.56
; Added new clause to deriv above and four exponent related methods

; Exercise 2.57

