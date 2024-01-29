#lang racket

(define ns (make-base-namespace))

(define bold
  (lambda str
    (format "<b>~a</b>" str)))

(define (eval-append lst result) 
  (cond [(null? lst) result]
        [(eval-append (rest lst) (string-append result (eval (first lst) ns)))]))
  
(define (what lst)
    (eval (first lst) ns))

(define (why)
  (bold "why"))

(define (dump lst)
  (let loop ([lst lst])
    (if (null? lst)
        (void)
        (let ([head (first lst)]
              [tail (rest lst)])
          (print head)
          (let ([test (eval head ns)])
            (display test))
          (loop tail)))))
;(dump '("a" (bold "allo")))
;(what '(bold "b"))
(what '((string-append "bob " "allo")))
(why)

;; errors: will fail if lst arguments has less than 2 items
#|
(define (extract-class lst)
  (let* ([len (length lst)]
        [penult (list-ref lst (- len 2))]
        [tag (first lst)])
    (if (equal? ''class penult)
        (let* ([values (apply string-append (drop-right (rest lst) 2))]
              [class-code (last lst)])
          (printf "<~a class=\"~a\">~a</~a>~%" tag class-code values tag))
        (printf "<~a>~a</~a>~%" tag (apply string-append (rest lst)) tag))))



(extract-class '("p" "hello" 'class "simple"))
(extract-class '("p" "hello"))
(extract-class '("p" '("hello" (bold "d")) 'class "simple"))
|#

        


;(eval-append '("a" (bold "allo")) "")

