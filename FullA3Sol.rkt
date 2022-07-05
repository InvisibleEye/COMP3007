;q1
;a
(define (compose . func)
  (define (composeRe arg)
    (if (null? func)
        arg
        (let ((f (car func))
              (g (cdr func)))
          (set! func g)
          (f (composeRe arg)))))

  composeRe)

(define square (lambda(x)(* x x)))
(define double (lambda(x)(+ x x)))
((compose square double) 3)


;b
(define (creat-list arg)
  (let iter ((arg arg)
             (y '()))
    (if (< arg 10)
        (cons arg y)
        (iter (quotient arg 10)
              (cons (remainder arg 10) y)))))
(define (endsWith x)
  (define (process arg)
    (if (= x (car (reverse (creat-list arg))))
        #t
        #f))
    process)
((endsWith 3) 1713)


;c
(define (newmap f)
  (define (mapHlp arg)
    (map f arg))
  mapHlp)
(define double-mapper (newmap (lambda(x)(* x 2))))
(double-mapper '(1 2 3 4))
(double-mapper '(10 20 30))

;d
(define (filter keep? f lst)
  (cond ((null? lst) '())
        ((equal? keep? (f (car lst)))
         (cons (car lst) (filter keep? f (cdr lst))))
        (else
         (filter keep? f (cdr lst)))))

(define (newfilter keep? f)
  (define (filterHlp arg)
    (filter keep? f arg))
  filterHlp)

(define bigNums   (newfilter #t (lambda(x)(> x 25))))
(define smallNums (newfilter #f (lambda(x)(> x 25))))
	
(bigNums '(10 20 30 40 50)) 
(smallNums '(10 20 30 40 50))

;e
(define (range a b)
    (if (= a b) (list a)
        (cons a (range (+ a 1) b))))
(define myfunc (newmap (lambda (x) (* x 3))))
(define positives (newfilter #t (lambda(x) (and (> x 0) (< x 10)))))

(myfunc (positives (range 0 10)))





;Q3
;a
(define tree-filter
  (lambda (arg T)
    (cond ((null? T) '())
          ((list? (car T)) (cons (tree-filter arg (car T))
                                 (tree-filter arg (cdr T))))
          ((arg (car T)) (cons (car T) (tree-filter arg (cdr T))))
          (else (tree-filter arg (cdr T))))))
(tree-filter even? '(1 (2 3) ((4 5) (6 7)) (((8 (9))))))

;b

;c
(define flattenList
  (lambda (t)
  (cond ((null? t) '())
        ((list? (car t)) (append (flattenList (car t))
                                (flattenList (cdr t))))
        (else (cons (car t) (flattenList (cdr t)))))))

(flattenList '(1 (2 3) ((4 5 6 (7)))(((8 (9))))))

;d
(define squish
  (lambda (T)
    (cond((null? T) '())
         ((and (list? (car T)) (null? (cdr T))) (cons (car T) (squish (cdr T))))
         ((and (list?  (car T)) (list? (cadr T))) (squish (cons (squish (append (squish (car T)) (squish (cadr T))))
                                                        (squish (cddr T)))))
         (else (cons (car T) (squish (cdr T)))))))

(squish '((1 2)(3) 4 (5 (6)(7))((8) 9)))
(squish '((a) (b) c (d)))
(squish '((5) (6) (9) (10)))      
(squish '((5 (6)(7))((8) 9)))
(squish '(5 (6 7) (8) 9))
(squish '(1 3 5 6))




;Q4
;a
(define-syntax stream-cons
    (syntax-rules ()
        ((stream-cons a b)(cons a (delay b)))))
(define (stream-car stream) 
    (car stream))
(define (stream-cdr stream)
    (force (cdr stream)))

(define (list->stream lis)
            (if (null? lis)
                '()
                (stream-cons (car lis)
                      (list->stream (cdr lis)))))

;b
(define (stream->list strm n)
  (cond ((null? strm) '())
        ((= n 0) (cons (stream-car strm) '()))
        (else (cons (stream-car strm) (stream->list (stream-cdr strm) (- n 1))))))
(stream->list (list->stream '(13 23 45 6 7)) 2)

;c
(define (stream-filter predicate stream)
    (cond ((null? stream) '())
          ((predicate (stream-car stream))
              (stream-cons (stream-car stream)
                           (stream-filter predicate 
                                          (stream-cdr stream))))
          (else (stream-filter predicate (stream-cdr stream)))))

(define (ints-starting-from i)
  (stream-cons i (ints-starting-from (+ i 1))))

(define odds
  (stream-filter (lambda (x) (not (= (modulo x 2) 0))) (ints-starting-from 1)))

;d
(define (repeated x)
  (stream-cons x (repeated x)))

;e

;f
(define (loan amnt rate payment)
  (cond ((<= amnt 0) (stream-cons 0 '()))
  (else (stream-cons amnt (loan (+ (- amnt payment) (* amnt rate)) rate payment)))))