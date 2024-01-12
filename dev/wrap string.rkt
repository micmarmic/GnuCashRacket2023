#lang racket
(define input "adf dfd d dddfd dfdffd dfdfdfd dffddfdf ffdffdfd")
(define line-len 10)

#|

split every 20

abcdef ghij klmnop resuve derirem erqrenqlk
1234567890123456789012345678901234567890123

word0: len 6 tot 6
word1: len 1 + 4 tot 11  <= add 1 for leading space!
word2: len 1 + 6 total 18
word3: len 1 + 6 total 25
too long
split after word 2 => '(2)

new start: resuve derirem erqrenqlk
word0: len 6 tot 6
word1: len 1 + 7 tot 14
word2: len 1 + 9 tot 24
too long
split after word1 -> actual word5 (previous split at 2, +1 for start, +1 current index) => '(2 4)

new start erqrenqlk
len is < linelen - nothing to do => '(2 5)

|#

(define input2 "You are searching all available Racket packages, including those that you may not have installed locally. ")
#|
You are searching
all available Racket
packages, including
those that you may
|#


;; take text (string) and return a string
;; no longer that max-len wrapped with \n before line-len
(define (wrap text line-len max-len)
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
                                
#|
(displayln input2)
(define list-words (string-split input2 " "))
(define (wrap-text text)
 (let loop  ([indices '(2 3 2 4 3)]             
             [words list-words]
             [result ""])
  (cond [(empty? indices)
         (string-append result "\n" (string-join words " "))]
        ;; no more line splits, return rest        
        [else
         (let* ([current-index (first indices)]
                [words-to-add (take words (first indices))]
                [newline (if (equal? result "") "" "\n")]
                [new-result (string-append result newline (string-join words-to-add " "))])
           (loop (rest indices) (drop words current-index) new-result))])))
|#

(displayln (wrap input2 20 200))
