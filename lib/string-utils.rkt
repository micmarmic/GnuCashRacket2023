#lang racket

(provide wrap-text)

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
