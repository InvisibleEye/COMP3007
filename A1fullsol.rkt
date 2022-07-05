;Question 1

;a.  1 + 2 - 3 + -4 - 5 + 6
(+ 1 2 (- 3) (- 4) (- 5) 6)

;b. 20-9+((46/2 + 2)*(20/5 - 1))
(+ (- 20 9) (* (+ (/ 46 2) 2) (- (/ 20 5) 1)))

;c
(/ (+ (/ 123 12) (* (- (/ 12 4) (/ 14 (+ 13 2))) 3)) 4)
;d
(+ (* (* 25 20) 4) (+ (/ 19 4) 3.15) (- (* 30 2) 47))


;q2
;a
(define (cube x) (* x x x))
;b
(define (f x) (- (* 2 (cube x)) 4))
;c
(define (g x) (- (f (* 2 x)) (cube x)))
;d
(define (h x) (+ (* 2 (g (/ x 2))) (f x)))

;e

;(h (* 2 3))
;(h 6)
;(+ (* 2 (g (/ 6 2))) (f 6))
;(+ (* 2 (g 3)) (- (* 2 (cube 6)) 4))
;(+ (* 2 (- (f (* 2 3)) (cube 3))) (- (* 2 (cube 6)) 4))
;(+ (* 2 (- (f 6) (* 3 3 3))) (- (* 2 (* 6 6 6)) 4))
;( + (* 2 (- (- (* 2 (cube 6)) 4) 27)) (- (* 2 216) 4))
;( + (* 2 (- (- (* 2 (* 6 6 6)) 4) 27)) (-  432 4))
;( + (* 2 (- (- (* 2 216) 4) 27)) 428 )
;( + (* 2 (- (- 432 4) 27)) 428 )
;( + (* 2 (- 428 27)) 428 )
;( + (* 2 401) 428 )
;(+ 802 428)
;1230


;d


;(h (* 2 3))
;(+ (* 2 (g (/ (* 2 3) 2))) (f (* 2 3)))
;(+ (* 2 (- (f (* 2 (/ (* 2 3) 2))) (cube (/ (* 2 3) 2)))) (- (* 2 (cube (* 2 3))) 4))
;(+ (* 2 (- (- (* 2 (cube (* 2 (/ (* 2 3) 2)))) 4) (* (/ (* 2 3) 2) (/ (* 2 3) 2) (/ (* 2 3) 2)))) (- (* 2 (*(* 2 3) (* 2 3) (* 2 3))) 4))
;(+ (* 2 (- (- (* 2 (* (* 2 (/ (* 2 3) 2)) (* 2 (/ (* 2 3) 2)) (* 2 (/ (* 2 3) 2)))) 4) (* (/ 6 2) (/ 6 2) (/ 6 2)))) (- (* 2 (* 6 6 6)) 4))
;(+ (* 2 (- (- (* 2 (* (* 2 (/ 6 2)) (* 2 (/ 6 2)) (* 2 (/ 6 2)))) 4) (* 3 3 3))) (- (* 2 216) 4))
;(+ (* 2 (- (- (* 2 (* (* 2 3) (* 2 3) (* 2 3))) 4) 27)) (- 432 4))
;(+ (* 2 (- (- (* 2 (* 6 6 6)) 4) 27)) 428)
;(+ (* 2 (- (- (* 2 216) 4) 27)) 428)
;(+ (* 2 (- (- 432 4) 27)) 428)
;(+ (* 2 (- 428 27)) 428)
;(+ (* 2 401) 428)
;(+ 802 428)
;1230

;q3
;qa
(define (standard-roundf x y)
  (let ((helpFunc (expt 10 y)))
    (cond ((>= .5 (- (ceiling (* helpFunc x)) (* helpFunc x)))
        (/ (ceiling (* helpFunc x)) helpFunc))
        (else (/ (round (* helpFunc x)) helpFunc)))))

;q2
;not finished
(define (area-triangle a b c)
  (define (calcS a b c)
    (/ (+ a b c) 2))
  (define (calcArea a b c)
    (sqrt (* (calcS a b c)
             (- (calcS a b c) a)
             (- (calcS a b c) b)
             (- (calcS a b c) c))))
  (define (valid-input?)
    (if (and (> (+ a b) c) (> (+ b c) a) (> (+ a c) b)) #t #f))
  (if (valid-input?) (standard-roundf (calcArea a b c) 3) 0))                                                  

;(area-triangle 3 3 5)
;(area-triangle 4 15 6)
;(area-triangle -2 5 4)


;q4
(display "Enter a year: ")
(define year (read))
(display "Enter a month: ")
(define month (read))
(display "Enter a day: ")
(define day (read))

(define (vali-date?)
  (define (leap-year?)
    (if (= (modulo year 4) 0) #t #f))
  (define (vali-year?)
    (if (and (integer? year) (>= year 1)) #t #f))
  (define (vali-month?)
    (if (and (integer? month) (>= month 1) (<= month 12)) #t #f))
  (define (vali-day?)
    (cond ((and (leap-year?) (vali-year?) (= month 2) (>= day 1) (<= day 29)) #t)
          ((and (not (leap-year?)) (vali-year?) (= month 2) (>= day 1) (<= day 28)) #t)
          ((and (vali-year?) (vali-month?)
                (or (= month 1) (= month 3) (= month 5) (= month 7) (= month 8) (= month 10) (= month 12))
                (<= day 1) (>= 31)) #t)
          ((and (vali-year?) (vali-month?)
                (or (= month 4) (= month 6) (= month 9) (= month 11))
                (<= day 1) (>= 30)) #t)
          (else #f)))
  (if (and (vali-year?) (vali-month?) (vali-day?)) #t #f))

;Q5
;a
;in aplicative-order it will behave like infinte loop. Since it will try to evaluate y brefor it proced farther.
;b
;in normal-order it should print out 1. Since y won't be evaluated until if statement become false.


;Q6

when b > 0 it evaluate => a + b
when b = 0 it evaluate => a * b
when b < 0 it evaluate => a / b
