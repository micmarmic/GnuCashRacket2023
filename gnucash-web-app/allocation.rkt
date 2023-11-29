 #lang racket

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
;;


(struct alloc-rec (ca us intl fixed other) #:transparent)

;;
;; HARDCODED ALLOC FOR NOW
;; 
(define current-alloc-hash
  (let ([hash (make-hash '())])
    (hash-set! hash "BMO" (alloc-rec 100 0 0 0 0))
    (hash-set! hash "HXS" (alloc-rec 0 100 0 0 0))
    (hash-set! hash "VEQT" (alloc-rec  30 43 27 0 0))
    (hash-set! hash "XIU" (alloc-rec 100 0 0 0 0))
    (hash-set! hash "ZDM" (alloc-rec 0 0 100 0 0))
    (hash-set! hash "MKB" (alloc-rec 0 0 0 100 0))
    (hash-set! hash "FAKE" (alloc-rec 20 20 20 20 20))
    hash))

; add all the value in an allo-hash
(define (total-alloc alloc)
  (+ (alloc-rec-ca alloc)
     (alloc-rec-us alloc)
     (alloc-rec-intl alloc)
     (alloc-rec-fixed alloc)
     (alloc-rec-other alloc)))

; return #t if all allocation totals are 100%
; if debug is on, display which record is wrong
(define (valid-alloc-hash? alloc-hash [debug #f])
  (define result (andmap (lambda (alloc) (= 100 (total-alloc alloc))) (hash-values alloc-hash)))
  (when (and (not result) debug)    
    (for ([key (in-list (hash-keys alloc-hash))])
      (define current-alloc (hash-ref alloc-hash key))
      (displayln current-alloc)
      (when (not (= 100 (total-alloc current-alloc)))
        (printf "Total is not 100: ~a ~a~%" key current-alloc))))
  result)

(valid-alloc-hash? current-alloc-hash #t)