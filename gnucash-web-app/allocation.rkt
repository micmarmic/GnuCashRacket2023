#lang racket

(provide file->alloc-hash
         valid-alloc-hash?
         alloc-rec
         alloc-rec-commodity
         alloc-rec-ca
         alloc-rec-us
         alloc-rec-intl
         alloc-rec-fixed
         alloc-rec-other
         alloc-rec<?
         as-percent-string)

(require rackunit)

(define HARD-CODED-ALLLOCATION-FILE "allocation-data.txt")
(define TEST-ALLLOCATION-FILE "tests/test-allocation-data.txt")


;; take 0.43 or 1 and return as 43% or 100%
(define (as-percent-string value)
  ; notice add % after ~a to get "n%"
  (format "~a%" (exact-round (* 100 value))))
  

#|
1. ASSET ALLOCATION

TARGET ALLOCATION  CA   US   INTL  OTHER
                  50%  50%      0      0

2.ALLOCATION TABLE CA   US   INTL  OTHER
Asset 1            100%
Asset 2                 100%
Asset 3             20%  20%   40%   20%

3. REPORT
                            ALLOCATED AMOUNTS
ACCOUNT 1      Amount    CA   US   INTL  OTHER
Asset 1        1000    1000
Asset 2        1000          1000
Totals         2000    1000  1000


ACCOUNT 2      Amount    CA   US   INTL  OTHER
Asset 1        1000    1000
Asset 3        1000     200   200   400    200 
Totals         2000    1200   200   400    200

--- SUMMARY ---
GRAND TOTALS   4000    2200  1200   400    200
ALLOCATION %   100%     55%   30%   10%     5%
TARGET     %   100%     50%   50%
DIFFERENCE %            -5%  +20%  -10%    -5%  
DIFFERENCE $           -$200 +$800 -$400  -$200

4. INPUT SCENARIOS

ASSET   AMOUNT
Asset1   1000

Display new summary?

--- REVISED SUMMARY ---
MET

|#

;;
;; STRUCT ALLOC-REC AND UTILS
;;(


(struct alloc-rec (commodity ca us intl fixed other) #:transparent)

(define (alloc-rec<? a1 a2)
  (string<? (alloc-rec-commodity a1) (alloc-rec-commodity a2)))


;; make alloc-rec records from elements in list
;; return an alloc-rec
(define (list->alloc-rec lst)
  (define (input->number input)
    (cond [(number? input) input]
          [(string? input) (string->number (string-trim input))]
          [else (error "error: input->number only processes strings and numbers, not [~a]" input)]))        
  (alloc-rec (first lst) ; first is string commodity, rest are numbers
             (input->number (second lst))
             (input->number (third lst))
             (input->number (fourth lst))
             (input->number (fifth lst))
             (input->number  (sixth lst))))

; add all the values in an allo-hash
(define (total-alloc alloc)
  (+ (alloc-rec-ca alloc)
     (alloc-rec-us alloc)
     (alloc-rec-intl alloc)
     (alloc-rec-fixed alloc)
     (alloc-rec-other alloc)))

; return #t if all allocation totals are 100%
; values are less than 1 and must total to 1
; if debug is on, display which record is wrong
(define (valid-alloc-hash? alloc-hash [debug #f])
  (define result (andmap (lambda (alloc) (= 1 (total-alloc alloc))) (hash-values alloc-hash)))
  (when (and (not result) debug)    
    (for ([key (in-list (hash-keys alloc-hash))])
      (define current-alloc (hash-ref alloc-hash key))
      (displayln current-alloc)
      (define total (total-alloc current-alloc))
      (when (not (= 1 total))
        (printf "Total is not 100: ~a ~a add up to ~a~%" key current-alloc total))))
  result)


;; ---------------------------
;;  LOAD ALLOCATION FROM FILE
;; ---------------------------

(define (file->alloc-hash file-path)
  (define alloc-hash (make-hash))
  (printf "DEBUG trying to get allocatin data from ~a~%" file-path)
  (call-with-input-file file-path    
    (lambda (in)
      (for ([line (in-lines in)])
        ; note: dispatch also advances the reading cursor in the file
        (unless (string-prefix? line ";")
          (define values (string-split line ","))
          (hash-set*! alloc-hash (first values) (list->alloc-rec values))))))
  alloc-hash)


 
  

;; -------
;;  TESTS
;; -------

;(define alloc-hash (file->alloc-hash HARD-CODED-ALLLOCATION-FILE))
;(valid-alloc-hash? alloc-hash #t)


(define bad-alloc-hash (hash "BAD-TOTAL" (alloc-rec "Test" 3 3 3 3 3)))
(check-false (valid-alloc-hash? bad-alloc-hash))
