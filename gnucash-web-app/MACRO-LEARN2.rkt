#lang racket
(define-syntax dbl
  (syntax-rules()
    [(dbl x) (+ x x)]))

(define-syntax dbl2
  (syntax-rules()
    [(dbl x) (* 2 x)]))

(define-syntax dbl3
  (syntax-rules()
    [(dbl x)
     (let ([y x])
       (+ y y))]))

(define-syntax dbl4
(syntax-rules ()
  [(dbl x) (let ([y 1])
             (* 2 x y))]))

(dbl 4)
(dbl2 4)
(dbl3 4)
(dbl (begin (print "hi") 42))
(dbl2 (begin (print "hi") 42))
(dbl3 (begin (print "hi") 42))

(dbl4 4)
(let ([y 7]) (dbl4 y))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-syntax-rule (assert expr)
  (when (not expr)
    (error 'assert "assertion failed: ~s" (quote expr))))

(assert (= 1 1))
;(assert (= 1 2))

(define-syntax-rule (eval-and-print expr)
  (printf "~a = ~a~%" (quote expr) expr))

(eval-and-print (+ 1 3))