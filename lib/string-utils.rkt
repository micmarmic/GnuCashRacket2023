#lang racket

(require rackunit)

(provide wrap-text list-subset)

;; take text (string) and return a string
;; no longer that max-len wrapped with \n before line-len
(define (wrap-text text max-len line-len)
  ; add to buffer with \n in front if buffer not empty
  (define (append-with-new-line text new-text)
    (string-append text (if (equal? "" text) "" "\n") new-text))
  (define (append-with-space text new-text)
    (string-append text (if (equal? "" text) "" " ") new-text))
  (define trunc-text (if (> (string-length text) max-len) (substring text 0 max-len) text))
  (define list-words (string-split trunc-text " "))
  (let loop ([remaining-words list-words]
             [buffer ""]
             [result ""])
    (cond [(empty? remaining-words)
           (append-with-new-line result buffer)]
          [else
           (let* ([current-word (first remaining-words)]
                  [length-current-word (string-length current-word)]
                  [length-buffer (string-length buffer)])
             (cond [(> (+ 1 length-current-word length-buffer) line-len)
                   ; buffer + word is too much, add buffer to result, make buffer equal to word
                    (loop (rest remaining-words)
                          current-word ; buffer
                          (append-with-new-line result buffer))]
                   [else (loop (rest remaining-words)
                               (append-with-space buffer current-word)
                               result)]))])))


;; not as string util but ...
;; given a list, return a list with the n items from the list starting as pos
(define (list-subset lst start-pos num-items)
  (define len (length lst))
  (when (< start-pos  0)
    (error "list-subset: cannot start at less than zero"))
  (when (> start-pos (sub1 len))
    (error "list-subset: given start-pos past end of list"))
  ; normalize: if num-items is too large, give all items you can, but no error
  (define pos (min start-pos (- len start-pos)))
  (let loop ([pos start-pos]
             [num num-items]
             [sublst '()]) ; items added in front, reverse when done
    (cond [(or (= pos len) (zero? num))
           (reverse sublst)]
          [else
           (loop (add1 pos) (sub1 num) (cons (list-ref lst pos) sublst))])))

(check-equal? (list-subset '(0 1 2 3 4 5) 0 0) '())
(check-equal? (list-subset '(0 1 2 3 4 5) 1 2) '(1 2))
(check-equal? (list-subset '(0 1 2 3 4 5) 5 1) '(5))