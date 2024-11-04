#lang sicp
(#%require sicp-pict)

; This is a linear combination for numbers
(define (linear-combination-number a b x y)
  (+ (* a x) (* b y)))

; This is a more generic combination
; for any data structure that is compatible
; with the add and mul procedures
(define (linear-combination-rat a b x y)
  (add-rat (mul-rat a x) (mul-rat b y)))

(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))

(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))

(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

(define numer car)
(define denom cdr)

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

; Exercise 2.1
; Very naive solution
(define (make-rat n d)
  (cond
    ; Both are negative
    ((and (< n 0) (< d 0))
     (let ((g (gcd (- n) (- d))))
       (cons (/ (- n) g) (/ (- d) g))))
    ; Both are positive
    ((and (>= n 0) (>= d 0))
     (let ((g (gcd n d)))
       (cons (/ n g) (/ d g))))
    ; n is positive and d is negative
    ((and (>= n 0) (< d 0))
     (let ((g (gcd n (- d))))
       (cons (/ (- n) g) (/ (- d) g))))
    ; n is negative and d is positive
    ((and (< n 0) (>= d 0))
     (let ((g (gcd (- n) d)))
       (cons (/ n g) (/ d g))))))

;(define (make-rat n d)
;  ((let ((g ((if (< d 0) - +) (abs (gcd n d)))))
;     (cons (/ n g) (/ d g)))))

; Exercise 2.2
(define (average x y)
  (/ (+ x y) 2))

(define make-point cons)
(define x-point car)
(define y-point cdr)
(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(define make-segment cons)
(define start-segment car)
(define end-segment cdr)

(define (midpoint-segment segment)
  (make-point
   (average (x-point (end-segment segment)) (x-point (start-segment segment)))
   (average (y-point (end-segment segment)) (y-point (start-segment segment)))))

; Exercise 2.3
; Note: I did not check to make sure that the given points
; will actually form a rectangle
(define (make-rect bottom-left top-right)
  (cons bottom-left top-right))

(define (rect-bottom-left rect)
  (car rect))

(define (rect-top-right rect)
  (cdr rect))

(define (rect-bottom-right rect)
  (make-point
   (x-point (rect-top-right rect))
   (y-point (rect-bottom-left rect))))

(define (rect-top-left rect)
  (make-point
   (x-point (rect-bottom-left rect))
   (y-point (rect-top-right rect))))

; Looking at solutions, I should have probably created
; a width and height helper function to simplify these
; calculations

(define (rect-perimeter rect)
  (* (+ (- (x-point (rect-top-right rect))
           (x-point (rect-bottom-left rect)))
        (- (y-point (rect-top-right rect))
           (y-point (rect-bottom-left rect))))
     2))
     
(define (rect-area rect)
  (* (- (x-point (rect-top-right rect))
        (x-point (rect-bottom-left rect)))
     (- (y-point (rect-top-right rect))
        (y-point (rect-bottom-left rect)))))

; Implementing cons, car and cdr without any data structures at all
; this is indistinguishable from the real version as long as the pairs
; are only accessed using these three procedures
; this still of programming is called message passing
;(define (cons x y)
;  (define (dispatch m)
;    (cond ((= m 0) x)
;          ((= m 1) y)
;          (else (error "Argument not 0 or 1: CONS" m))))
;  dispatch)
;(define (car z) (z 0))
;(define (cdr z) (z 1))

; Exercise 2.4
(define (my-cons x y)
  (lambda (m) (m x y)))
(define (my-car z)
  (z (lambda (p q) p)))
(define (my-cdr z)
  (z (lambda (p q) q)))

; Exercise 2.5
(define (exp base n)
  (define (iter x result)
    (if (= 0 x)
        result
        (iter (- x 1) (* base result))))
  (iter n 1))

; Hacky
(define (count-divisions-until-0-rem num divisor)
  (define (iter num result)
    (if (not (integer? num))
        (- result 1)
        (iter (/ num divisor) (+ result 1))))
  (iter num 0))

(define (my-cons-2 a b) (* (exp 2 a) (exp 3 b)))
(define (my-car-2 c) (count-divisions-until-0-rem c 2.0))
(define (my-cdr-2 c) (count-divisions-until-0-rem c 3.0))

; Exercise 2.6
; Church numerals
(define zero (lambda (f) (lambda (x) x)))
(define one  (lambda (f) (lambda (x) (f x))))
(define two  (lambda (f) (lambda (x) (f (f x)))))
(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

; Extended Exercise 2.1.4
(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (mul-interval
   x
   (if (or (< 0 (lower-bound y)) (< 0 (upper-bound y)))
        (error "Bound of y spans 0, cannot compute")
        (make-interval (/ 1.0 (upper-bound y))
                  (/ 1.0 (lower-bound y))))))

; Exercise 2.7
(define (make-interval a b) (cons a b))
(define (lower-bound interval) (car interval))
(define (upper-bound interval) (cdr interval))

; Exercise 2.8
(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))

; Exercise 2.10
; Modify code to handle dividing by interval that spans zero, done above

; Exercise 2.11
; TODO

; Exercise 2.12
(define (make-center-percent center percent)
  (let ((tolerance (* center (/ percent 100))))
    (make-interval
     (- center tolerance)
     (+ center tolerance))))

; Exercise 2.13, 14, 15, 16
; TODO


; One way to represent a sequence:
; (cons 1
;       (cons 2
;             (cons 3
;                   (cons 4 nil))))

; The car of each element represents the item in the chain, and
; the cdr is the next pair in the chain. The final cdr uses nil
; to distingush that the value is not a pair, and thus signals
; the end of the sequence. This is a list

; The above is equivalent to the following:
; (list 1 2 3 4)

(define one-through-four (list 1 2 3 4))

; Procedure to return n'th element of a list
(define (list-ref items n)
  (define (iter result count)
    (if (= count 0)
      (car result)
      (iter (cdr result) (- count 1))))
  (iter items n))

#| (define (length items) |#
#|   (if (null? items) |#
#|     0 |#
#|     (+ 1 (length (cdr items))))) |#

(define (length-iter items)
  (define (iter result items)
    (if (null? items)
      result
      (iter (+ result 1) (cdr items))))
  (iter 0 items))

#| (define (append list1 list2) |#
#|   (if (null? list1) |#
#|     list2 |#
#|     (cons (car list1) (append (cdr list1) list2)))) |#

; Exercise 2.17
(define (last-pair items)
  (if (null? (cdr items))
    (car items)
    (last-pair (cdr items))))
; Only improvement here would be to use
; let to avoid calling cdr on items twice

; Exercise 2.18
#| (define (reverse items) |#
#|   (define (iter items result) |#
#|     (if (null? items) |#
#|       result |#
#|       (iter (cdr items) (cons (car items) result)))) |#
#|   (iter items nil)) |#

; Exercise 2.19
(define (cc amount coin-values)
  (cond ((= amount 0) 1)
    ((or (< amount 0) (no-more? coin-values)) 0)
    (else
     (+ (cc amount
            (except-first-denomination
             coin-values))
        (cc (- amount
               (first-denomination
                coin-values))
            coin-values)))))

; Order does not affect result but affects the speed of computation
; smaller valued coins first will make it take longer
(define (first-denomination coin-values)
  (car coin-values))
(define (except-first-denomination coin-values)
  (cdr coin-values))
(define (no-more? coin-values)
  (null? coin-values))

; Exercise 2.20
(define (even? x) (= (remainder x 2) 0))
(define (odd? x) (not (even? x)))

(define (same-parity? x y)
  (or (and (even? x) (even? y))
      (and (odd? x) (odd? y))))

(define (same-parity x . items)
  (define (iter result items)
    (if (null? items)
      result
      (if (same-parity? x (car items))
        (iter (cons (car items) result) (cdr items))
        (iter result (cdr items)))))
  (reverse (iter (cons x nil) items)))

;---------------------------------------------------

#| (define (map proc items) |#
#|   (if (null? items) |#
#|     nil |#
#|     (cons (proc (car items)) |#
#|           (map proc (cdr items))))) |#

(define (scale-list items factor)
  (map (lambda (x) (* x factor))
       items))

; Exercise 2.21

(define (square x) (* x x))

; (define (square-list items)
;   (if (null? items)
;     nil
;     (cons (square (car items))
;           (square-list (cdr items)))))
; 
; (define (square-list items)
;   (map (lambda (x) (square x)) items))

; Exercise 2.22
; TODO

; Exercise 2.23
(define (for-each proc items)
  (if (null? items)
    #t
    (and (proc (car items)) (for-each proc (cdr items)))))
; --------------------------------------------------------

#| (define (count-leaves items) |#
#|   (cond |#
#|     ((null? items) 0) |#
#|     ((not (pair? items)) 1) |#
#|     (else (+ |#
#|            (count-leaves (car items)) |#
#|            (count-leaves (cdr items)))))) |#

; Exercise 2.24
; Done on paper

; Exercise 2.25
(define list-1 (list 1 3 (list 5 7) 9))
(= 7 (car (cdr (car (cdr (cdr list-1))))))

(define list-2 (list (list 7)))
(= 7 (car (car list-2)))

(define list-3 (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))
; TODO

; Exercise 2.26
(define x (list 1 2 3))
(define y (list 4 5 6))

#| (newline) |#
#| (display (append x y)) |#
#| (newline) |#
#| (display (cons x y)) |#
#| (newline) |#
#| (display (list x y)) |#

; Exercise 2.27
; Takes a list as argument and returns as its value
; the list with its elements reversed and all the
; sublists deep-reversed

; For reference:
#| (define (reverse items) |#
#|   (define (iter items result) |#
#|     (if (null? items) |#
#|       result |#
#|       (iter (cdr items) (cons (car items) result)))) |#
#|   (iter items nil)) |#

(define (deep-reverse items)
  (define (iter items result)
    (if (null? items)
      result
      (let ((x (car items)))
        (if (pair? x)
          (iter (cdr items) (cons (deep-reverse (car items)) result))
          (iter (cdr items) (cons (car items) result))))))
  (iter items nil))

; Exercise 2.28
(define (fringe tree)
  (define (iter x result)
    (cond
      ((null? x) result)
      ((not (pair? x)) (cons x result))
      (else (iter (car x) (iter (cdr x) result)))))
  (iter tree nil))

; Exercise 2.29
#| (define (make-mobile left right) |#
#|   (list left right)) |#

#| (define (make-branch length structure) |#
#|   (list length structure)) |#

; d. Changing constructors
; only have to change selectors to make program work
; with altered definitions, which is good because
; it means that the procedures that operate on branches
; and mobiles do not rely on the internal representation
; of them
(define (make-mobile left right) (cons left right))
(define (make-branch length structure) (cons length structure))

; a.
(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  #| (car (cdr mobile))) |#
  (cdr mobile))

(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  #| (car (cdr branch))) |#
  (cdr branch))

; b.
(define (total-weight mobile)
  (cond
    ((null? mobile) 0)
    ((not (pair? mobile)) mobile)
    (else (+ (total-weight (branch-structure (left-branch mobile)))
             (total-weight (branch-structure (right-branch mobile)))))))

(define (torque branch)
  (* (branch-length branch) (total-weight (branch-structure branch))))
       
(define (balanced? mobile)
  (if (not (pair? mobile))
    true
    (let ((left (left-branch mobile))
          (right (right-branch mobile)))
      (and (= (torque left) (torque right))
           (balanced? (branch-structure left))
           (balanced? (branch-structure right))))))

; Mapping over trees
#| (define (scale-tree tree factor) |#
#|   (cond |#
#|     ((null? tree) nil) |#
#|     ((not (pair? tree)) (* tree factor)) |#
#|     (else (cons (scale-tree (car tree) factor) |#
#|                 (scale-tree (cdr tree) factor))))) |#
      
; Implementing by regarding the tree as a sequence of subtrees
; and using map

(define (scale-tree tree factor)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
           (scale-tree sub-tree factor)
           (* sub-tree factor)))
       tree))

; Exercise 2.31

#| (define (square-tree tree) |#
#|   (cond |#
#|     ; This case is for the nil at the end of |#
#|     ; a list |#
#|     ((null? tree) nil) |#
#|     ((not (pair? tree)) (square tree)) |#
#|     (else (cons (square-tree (car tree)) |#
#|                 (square-tree (cdr tree)))))) |#


(define (tree-map proc tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
           (tree-map proc sub-tree)
           (proc sub-tree)))
       tree))

(define (square-tree tree) (tree-map square tree))

; Exercise 2.32
(define (subsets s)
  (if (null? s)
    (list nil)
    (let ((rest (subsets (cdr s))))
      (append rest (map (lambda (x) (cons (car s) x)) rest)))))

; Section 2.2.3
(define (filter predicate sequence)
  (cond 
    ((null? sequence) nil)
    ((predicate (car sequence))
     (cons (car sequence)
           (filter predicate (cdr sequence))))
    (else (filter predicate (cdr sequence)))))
    
(define (accumulate op initial sequence)
  (if (null? sequence)
    initial
    (op (car sequence)
        (accumulate op initial (cdr sequence)))))

(define (enumerate-interval low high)
  (if (> low high)
    nil
    (cons low (enumerate-interval (+ low 1) high))))

(define (enumerate-tree tree)
  (cond
    ((null? tree) nil)
    ((not (pair? tree)) (list tree))
    (else (append (enumerate-tree (car tree))
                  (enumerate-tree (cdr tree))))))

(define (sum-odd-squares tree)
  (accumulate + 0 (map square (filter odd? (enumerate-tree tree)))))

(define (fib n)
; Basicaly the algorithm is recast as one
; with three state variables to make it
; iterative instead of recursive
  (define (fib-iter a b count)
    (if (> count n)
      b
      (fib-iter (+ a b) a (+ count 1))))
  (fib-iter 1 0 1))

(define (even-fibs n)
  (accumulate
   cons nil (filter even? (map fib (enumerate-interval 0 n)))))

(define (list-fib-squares n)
  (accumulate
   cons
   nil
   (map square (map fib (enumerate-interval 0 n)))))

(define (product-of-squares-of-odd-elements sequence)
  (accumulate * 1 (map square (filter odd? sequence))))

; Exercise 2.33
#| (define (map p sequence) |#
#|   (accumulate |#
#|    ; x is the current element |#
#|    ; y is the rest of the sequence |#
#|    (lambda (x y) |#
#|      (if (null? x) |#
#|        nil |#
#|        (cons (p x) y))) |#
#|    nil |#
#|    sequence)) |#

; This one is a bit mind-bending
(define (append seq1 seq2)
  (accumulate cons (accumulate cons nil seq2) seq1))

(define (length sequence)
  (accumulate (lambda (x y)
                (if (null? x)
                  0
                  (+ 1 y)))
                0 sequence))

; Exercise 2.34
(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms)
                (+ this-coeff (* x higher-terms)))
              0
              coefficient-sequence))

; Exercise 2.35
(define (count-leaves tree)
  (accumulate + 0
              (map
               (lambda (t)
                 (cond
                   ((null? t) 0)
                   ((pair? t) (count-leaves t))
                   (else 1)))
               tree)))
                   
; Exercise 2.36
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
    nil
    (cons (accumulate op init (map (lambda (s) (car s)) seqs))
          (accumulate-n op init (map (lambda (s) (cdr s)) seqs)))))

; Exercise 2.37
(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (row) (dot-product row v)) m))

(define (transpose mat)
  (accumulate-n cons nil mat))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (v) (matrix-*-vector cols v)) m)))

; Exercise 2.38
; Note: "accumulate" is equivalent to fold-right
(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
      result
      (iter (op result (car rest))
            (cdr rest))))
  (iter initial sequence))
; Completed on paper
; For fold-left and fold-right to produce the same result,
; operation needs to be associative

; Exercise 2.39
(define fold-right accumulate)

; Using fold-right
#| (define (reverse sequence) |#
#|   (fold-right (lambda (first already-reversed) |#
#|                 (append already-reversed (list first))) |#
#|                 nil |#
#|                 sequence)) |#

; Using fold-left
#| (define (reverse sequence) |#
#|   (fold-left (lambda (result first) |#
#|                (cons first result)) |#
#|              nil |#
#|              sequence)) |#

; This is a common pattern called flatmapping
#| (define n 6) |#
#| (display (accumulate |#
#|  append nil (map (lambda (i) |#
#|                    (map (lambda (j) (list i j)) |#
#|                         (enumerate-interval 1 (- i 1)))) |#
#|                    (enumerate-interval 1 n)))) |#

; From Chapter 1:
(define (smallest-divisor n) (find-divisor n 2))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
    ((divides? test-divisor n) test-divisor)
    (else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b) (= (remainder b a) 0))
(define (prime? a) (= (smallest-divisor a) a))

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

#| (define (prime-sum-pairs n) |#
#|   (map make-pair-sum |#
#|        (filter prime-sum? (flatmap |#
#|                            (lambda (i) |#
#|                              (map (lambda (j) (list i j)) |#
#|                                   (enumerate-interval 1 (- i 1)))) |#
#|                            (enumerate-interval 1 n))))) |#

(define (remove element-to-remove sequence)
  (filter (lambda (element) (not (= element element-to-remove)))
          sequence))

(define (permutations s)
  (if (null? s)
    (list nil)
    (flatmap (lambda (x)
               (map (lambda (p) (cons x p))
                    (permutations (remove x s))))
               s)))

; Exercise 2.40
; Generate pairs (i,j) with 1 <= j < i <= n
(define (unique-pairs n)
  (flatmap (lambda (i)
             (map (lambda (j) (list i j))
                  (enumerate-interval 1 (- i 1))))
             (enumerate-interval 2 n)))

; Simplifying prime-sum-pairs:
(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum? (unique-pairs n))))

; Exercise 2.41
; Write a procedure to find ordered triples
; of distinct integers i j k less than or equal to n
; that sum to a given integer s

; Get all ordered triples (i,j,k) where 
; 1 <= i < j < k <= n
(define (ordered-triples n)
  (flatmap (lambda (k)
             (flatmap (lambda (j)
                    (map (lambda (i) (list i j k))
                         (enumerate-interval 1 (- j 1))))
                  (enumerate-interval 2 (- k 1))))
           (enumerate-interval 3 n)))

(define (ordered-triple-sums n s)
  (filter (lambda (triple)
            (= (accumulate + 0 triple) s))
          (ordered-triples n)))

; Exercise 2.42
; TODO

; Exercise 2.43
; TODO

; 2.2.4
#| (define (flipped-pairs painter) |#
#|   (let ((painter2 (beside painter (flip-vert painter)))) |#
#|     (below painter2 painter2))) |#

#| (define wave4 (flipped-pairs wave)) |#

#| (define (right-split painter n) |#
#|   (if (= n 0) |#
#|     painter |#
#|     (let ((smaller (right-split painter (- n 1)))) |#
#|       (beside painter (below smaller smaller))))) |#

; Exercise 2.44
#| (define (up-split painter n) |#
#|   (if (= n 0) |#
#|     painter |#
#|     (let ((smaller (up-split painter (- n 1)))) |#
#|       (below painter (beside smaller smaller))))) |#

; Apply four different operations to a painter
; and arrange them in a square
#| (define (square-of-four tl tr bl br) |#
#|   (lambda (painter) |#
#|     (let ((top (beside (tl painter) (tr painter))) |#
#|           (bottom (beside (bl painter) (br painter)))) |#
#|       (below bottom top)))) |#

#| (define (square-limit painter n) |#
#|   (let ((combine4 (square-of-four flip-horiz identity |#
#|                                   rotate180 flipvert))) |#
#|     (combine4 painter))) |#

; Exercise 2.45
#| (define (split painter orig-placer split-placer) |#
#|   (if (= n 0) |#
#|     painter |#
#|     (let ((smaller (right-split painter (- n 1)))) |#
#|       (orig-placer painter (split-placer smaller smaller))))) |#

#| (define (right-split (split beside below))) |#
#| (define (up-split (split below beside))) |#

; Frame can be described by three vectors,
; an origin vector and two edge vectors,
; if edges are perpendicular, the frame will be rectangular,
; otherwise it will be a general parallelogram

#| (define (frame-coord-map frame) |#
#|   (lambda (v) |#
#|     (add-vect |#
#|      (origin-frame frame) |#
#|      (add-vect (scale-vect (xcor-vect v) (edge1-frame frame)) |#
#|                (scale-vector (ycor-vect v) (edge2-frame frame)))))) |#

; Exercise 2.46
(define (make-vect x y)
  (cons x y))

(define (xcor-vect vect) (car vect))
(define (ycor-vect vect) (cdr vect))

(define (add-vect v1 v2)
  (make-vect (+ (xcor-vect v1)
                (xcor-vect v2))
             (+ (ycor-vect v1)
                (ycor-vect v2))))

(define (sub-vect v1 v2)
  (make-vect (- (xcor-vect v1)
                (xcor-vect v2))
             (- (ycor-vect v1)
                (ycor-vect v2))))

(define (scale-vect s v)
  (make-vect (* s (xcor-vect v))
             (* s (ycor-vect v))))

; Exercise 2.47

; a.
#| (define (make-frame origin edge1 edge2) |#
#|   (list origin edge1 edge2)) |#

#| (define (get-frame-origin frame) |#
#|   (car frame)) |#

#| (define (get-frame-edge1 frame) |#
#|   (cadr frame)) |#

#| (define (get-frame-edge2 frame) |#
#|   (caddr frame)) |#

; b.
(define (make-frame origin edge1 edge2)
  (cons origin (cons edge1 edge2)))

(define (get-frame-origin frame)
  (car frame))

(define (get-frame-edge1 frame)
  (cadr frame))

(define (get-frame-edge2 frame)
  (cddr frame))

; Exercise 2.48
#| (define (make-segment start-segment end-segment) |#
#|   (cons start-segment end-segment)) |#

#| (define (start-segment segment) (car segment)) |#
#| (define (end-segment segment) (cdr segment)) |#

; Exercise 2.49
(define (draw-line segment1 segment2)
  '())
(define (segments->painter segment-list)
  (lambda (frame)
    (for-each
     (lambda (segment)
       (draw-line
        ((frame-coord-map frame)
         (start-segment segment))
        ((frame-coord-map frame)
         (end-segment segment))))
     segment-list)))

(define (outline-painter frame)
  (let ((origin (make-vect 0.0 0.0))
        (top-left-vect (make-vect 0.0 1.0))
        (top-right-vect (make-vect 1.0 1.0))
        (bottom-right-vect (make-vect 1.0 0.0)))
    (segments->painter
     (list
      (make-segment origin top-left-vect)
      (make-segment top-left-vect top-right-vect)
      (make-segment top-right-vect bottom-right-vect)
      (make-segment bottom-right-vect origin)))))

(define (x-painter frame)
  (let ((origin (make-vect 0.0 0.0))
        (top-left-vect (make-vect 0.0 1.0))
        (top-right-vect (make-vect 1.0 1.0))
        (bottom-right-vect (make-vect 1.0 0.0)))
    (segments->painter
     (list
      (make-segment origin top-right-vect)
      (make-segment top-left-vect bottom-right-vect)))))

(define (diamond-painter frame)
  (let ((bottom (make-vect 0.5 0.0))
        (right (make-vect 1.0 0.5))
        (top (make-vect 0.5 1.0))
        (left (make-vect 0.0 0.5)))
    (segments->painter
     (list
      (make-segment bottom right)
      (make-segment right top)
      (make-segment top left)
      (make-segment left bottom)))))

; -------------------------------------------------------------


(define (transform-painter painter origin corner1 corner2)
  (lambda (frame)
    (let ((m (frame-coord-map frame)))
      (let ((new-origin (m origin)))
        (painter (make-frame
                  new-origin
                  (sub-vect (m corner1) new-origin)
                  (sub-vect (m corner2) new-origin)))))))

(define (flip-vert painter)
  (transform-painter painter
                     ; New origin
                     (make-vect 0.0 1.0)
                     ; New end of edge1
                     (make-vect 1.0 1.0)
                     ; New end of edge2
                     (make-vect 0.0 0.0)))

(define (shrink-to-upper-right painter)
  (transform-painter painter
                     (make-vect 0.5 0.5)
                     (make-vect 1.0 0.5)
                     (make-vect 0.5 1.0)))

(define (rotate90 painter)
  (transform-painter painter
                     (make-vect 1.0 0.0)
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 0.0)))

(define (squash-inwards painter)
  (transform-painter painter
                     (make-vect 0.0 0.0)
                     (make-vect 0.65 0.35)
                     (make-vect 0.35 0.65)))

(define (beside painter1 painter2)
  (let ((split-point (make-vect 0.5 0.0)))
    (let ((paint-left
           (transform-painter
            painter1
            (make-vect 0.0 0.0)
            split-point
            (make-vect 0.0 1.0)))
          (paint-right
           (transform-painter
            painter2
            split-point
            (make-vect 1.0 0.0)
            (make-vect 0.5 1.0)))))
    (lambda (frame)
      (paint-left frame)
      (paint-right frame))))

; Exercise 2.50
(define (flip-horiz painter)
  (transform-painter painter
                     (make-vect 1.0 0.0)
                     (make-vect 0.0 0.0)
                     (make-vect 1.0 1.0)))

(define (rotate180 painter)
  (transform-painter painter
                     (make-vect 1.0 1.0)
                     (make-vect 1.0 0.0)
                     (make-vect 0.0 1.0)))

(define (rotate270-ccw painter)
  (transform-painter painter
                     (make-vect 1.0 0.0)
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 0.0)))

; Exercise 2.51
(define (below painter1 painter2)
  (let ((split-point (make-vect 0.0 0.5)))
    (let ((paint-top
           (transform-painter
            painter2
            split-point
            (make-vect 1.0 0.5)
            (make-vect 0.0 1.0)))
          (paint-bottom
           (transform-painter
            painter1
            (make-vect 0.0 0.0)
            (make-vect 1.0 0.0)
            split-point))))
    (lambda (frame)
      (paint-bottom frame)
      (paint-top frame))))

; Exercise 2.52
; a.
; TODO

; -------------------------------------
;
; 2.3
;
; -------------------------------------



