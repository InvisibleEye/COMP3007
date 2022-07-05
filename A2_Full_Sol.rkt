;Q1
;a
(define (cube-root y)
  (define (squre x)
    (* x x))
  (define (cube x)
    (* x x x))
  (define (good-enough? guess x)
    (< (abs (- (cube guess) x)) 0.001))
  (define (average x y)
    (/ (+ x y) 3))
  (define (improve guess x)
    (average (/ x (squre guess)) (* 2 guess)))
  (define (cubert-iteration guess x)
    (if (good-enough? guess x)
        guess
        (cubert-iteration (improve guess x) x)))
  (cubert-iteration 1.0 y))

;(cube-root 27.7)

;b

(define (cube-root y good-enough?)
  (define (squre x)
    (* x x))
  (define (average x y)
    (/ (+ x y) 3))
  (define (improve guess x)
    (average (/ x (squre guess)) (* 2 guess)))
  (define (cubert-iteration guess x)
    (if (good-enough? guess x)
        guess
        (cubert-iteration (improve guess x) x)))
  (cubert-iteration 1.0 y))
(define (cube x)
  (* x x x))

(define (good-enough-to-0.1? guess x)
  (< (abs (- (cube guess) x)) 0.1))
(define (good-enough-to-0.001? guess x)
  (< (abs (- (cube guess) x)) 0.01))
(define (good-enough-to-0.0001? guess x)
  (< (abs (- (cube guess) x)) 0.0001))


;(cube-root 27 good-enough-to-0.1?)
;(cube-root 27 good-enough-to-0.001?)
;(cube-root 27 good-enough-to-0.0001?)

;c

(define (cube-root2 y)
  (define (squre x)
    (* x x))
  (define (cube x)
    (* x x x))
  (define (good-enough? guess x)
    (< (abs (- (cube guess) x)) 0.001))
  (define (average x y)
    (/ (+ x y) 3))
  (define (improve guess x)
    (average (/ x (squre guess)) (* 2 guess)))
  (define (cubert-iteration guess x)
    (new-if (< x 0) (- x) x))
  (cubert-iteration 1.0 y))
(define (new-if predicate consequent alternate)
        (cond (predicate consequent)
              (else alternate)))

;(cube-root2 8)

;No it doesn't work.


;Q2
;a

(define (product a b term next)
    (if (>= a b)
        b
        (* (term a)
           (product (next a) b term next))))
(define (term n) n)
(define (next n) (+ n 1))
;(product 0 5 (lambda(x)x)(lambda(x)(+ x 1))) 

;b
(define (product-it a b term next)
  (define (pro-it x y count)
    (cond ((and (= a 1) (= count (+  1 (- b a)))) x)
          ((and (not (= a 1)) (= count (+ 2 (- b a)))) x)
          ((and (not (= a 1)) (= count 0))
           (pro-it (* x y) x (next count)))
          ((and (not (= a 1)) (= count 1))
           (pro-it x (next y) (next count)))
          (else (pro-it (* x y) (next y) (next count)))))

  (cond ((and (< a 0) (> b 0)) 0)
        ((>= a b) b)
        (else (pro-it a 1 0))))
       
(define (term x ) x)
(define (next x) (+ x 1))
;(product-it 7 8 (lambda(x)x)(lambda(x)(+ x 1)))

;c
(define (cube x) (* x x x))
;i
(product 1 2 (lambda(a)(+ (* a a) (* (- 3) a) 7)) (lambda(a)(+ a 1)))
;ii
(product 0 10 (lambda(b) (cube (+ (* 2 b) 1))) (lambda (b) (+ b 1)))  

;Q3
;a
(define (f n)
    (cond  ((< n 3) n)
           (else (+ (f (- n 1))
                    (* 2 (f (- n 2)))
                    (* 3 (f (- n 3)))))))
(f 5)
;this is a tree recursion. So some values have been calculated multiple times.

;(f 5)
;(+ (f (- 5 1)) (* 2 (f (- 5 2))) (* 3 (f (- 5 3))))
;(+ (f 4) (* 2 (f 3)) (* 3 (f 2)))
;(+ (+ (f (- 4 1)) (* 2 (f (- 4 2))) (* 3 (f (- 4 3)))) (* 2 (+ (f (- 3 1)) (* 2 (f (- 3 2))) (* 3 (f (- 3 3))))) (* 3 5))
;(+ (+ (f 3) (* 2 (f 2)) (* 3 (f 1))) (* 2 (+ (f 2) (* 2 (f 1)) (* 3 (f 0)))) 15)
;(+ (+ (+ (f (- 3 1)) (* 2 (f (- 3 2))) (* 3 (f (- 3 3)))) (* 2 5) (* 3 5)) (* 2 (+ 5 (* 2 5) (* 3 5))) 15)
;(+ (+ (+ (f 2) (* 2 (f 1)) (* 3 (f 0))) 10 15) (* 2 (+ 5 10 15)) 15)
;(+ (+ (+ 5 (* 2 5) (* 3 5)) 10 15) (* 2 30) 15)
;(+ (+ (+ 5 10 15) 10 15) 60 15)
;(+ (+ 30 10 15) 60 15)
;(+ 55 60 15)


(define (f-it x)
  (define (f-iteration count n1 n2 n3)
    (if (> count x) n1
        (f-iteration (+ count 1) (+ n1 n2 n3) n1 n2)))
  (f-iteration 1 3 0 0))
(f-it 5)


;Q4
(define (bars wallet price wrappers)
  (define (canBuy)
    (/ wallet price))
  (define (chocoWrapGet)
    (- (canBuy) (* (floor (/ (canBuy) wrappers)) wrappers)))
  (define (chocoWrap)
    (cond ((< (canBuy) wrappers) (canBuy))
          ((>= (canBuy) wrappers)(/ (canBuy) wrappers))))
  (define (totalWrapChoco)
    (cond((>= (+ (chocoWrap) (chocoWrapGet)) wrappers)
          (/ (+ (chocoWrap) (chocoWrapGet)) wrappers))
         (else 0)))
  (floor (+ (canBuy) (chocoWrap) (totalWrapChoco))))
;(bars 10 2 2)