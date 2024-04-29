
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below

;problem 1
(define (sequence low high stride) 
    (if (> low high) 
    null
    (cons low (sequence (+ low stride) high stride))))

;problem 2
(define (string-append-map xs suffix)
    (map (lambda (x) (string-append x suffix)) xs))

;problem 3
(define (list-nth-mod xs n) (
    cond [(< n 0) (error "list-nth-mod: negative number")]
        [(null? xs) (error "list-nth-mod: empty list")]
        [#t (car (list-tail xs (remainder n (length xs))))]
))

;problem 4
(define (stream-for-n-steps s n) (
    if (>= 0 n) 
        null 
        (cons (car (s)) (stream-for-n-steps (cdr (s)) (- n 1))))
)

;problem 5
(define funny-number-stream
  (letrec ([f (lambda (x)
                (cons (if (= 0 (remainder x 5))
                          (- x)
                          x )
                      (lambda () (f (+ x 1)))))])
    (lambda () (f 1))))

;problem 6
(define dan-then-dog 
  (letrec (
    [dog (lambda () (cons "dog.jpg" dan))]
    [dan (lambda () (cons "dan.jpg" dog))]
    )
    dan
  )
)

;problem 7
(define (stream-add-zero s) 
    (lambda () (cons (cons 0 (car (s))) (stream-add-zero (cdr(s)))))
)

;problem 8
(define (cycle-lists xs ys) 
  (letrec (
    [f (lambda (x) 
        (cons (cons (list-nth-mod xs x) (list-nth-mod ys x)) 
        (lambda () (f (+ x 1)))))
    ])
    (lambda () (f 0))
  )
)

;problem 9
(define (vector-assoc val vector) (
  letrec (
    [l (vector-length vector)]
    [f (lambda (v vec n) 
      (cond 
        [(>= n l) #f]
        [(not (pair? (vector-ref vec n))) (f v vec (+ n 1))]
        [(equal? v (car (vector-ref vec n))) (vector-ref vec n)]
        [#t  (f v vec (+ n 1))]
        ))])
    (f val vector 0)
  )
)

;problem 10
(define (cached-assoc xs n)
  (let ([cache (make-vector n #f)]
    [next-place 0])
    (lambda (v) (
        let ([cached (vector-assoc v cache)])
        (if cached
            cached
            (let ([ans (assoc v xs)])
            (if ans 
              (begin
                (vector-set! cache next-place ans)
                (set! next-place (if (< next-place (- n 1)) 
                                        (+ next-place 1) 0))
                ans) 
            ans
)))))))