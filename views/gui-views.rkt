#lang racket

(require
  (file "../lib/finance.rkt")
  (file "../lib/finance.rkt")
  )

(provide 
         roi-column-headers
         get-roi-table-data
         )


(define roi-column-headers '("Commodity" "Cost" "Market Value" "Gain Loss" "Roi"))

;; return a dictionary with key=account name (can be "Total")
;; and values is nested list of row-column for ROI report ready for GUI table view
;; pre-convert all numbers to string
;; example
;; (list
;;    (list "BCE 	"25689.26" 	"23509.20" 	"-2180.06" 	"-8.49%")
;;    (list "HXS" 	"40990.67" 	"63277.40" 	"22286.73" 	"54.37%"))
(define (get-roi-table-data gnucash-data date [alloc-hash null])
  ; roi-on-date returns (list account-roi)
  ;(with-handlers ([exn:fail? (λ (e) (exn-handler e))])
    (let* ([master-list-roi (roi-on-date gnucash-data date alloc-hash)]
           [grand-total-line (calc-grand-total-list-account-roi master-list-roi)])
      (define hash-result (make-hash))
      (for/list ([an-account-roi (in-list master-list-roi)])
        (define account-name (send (account-roi-account an-account-roi) get-name))
        (define lines-for-account
          (for/list ([a-roi-line (account-roi-child-roi-lines an-account-roi)])
            (list
             (roi-line-commo-id a-roi-line)
             (real->decimal-string(roi-line-cost a-roi-line))
             (real->decimal-string(roi-line-value a-roi-line))
             (real->decimal-string(roi-line-gain-loss a-roi-line))
             (real->decimal-string(roi-line-roi a-roi-line)))))
        (hash-set*! hash-result account-name lines-for-account)
        (hash-set*! hash-result "GRAND TOTAL"
                    (list (list
             (roi-line-commo-id grand-total-line)
             (real->decimal-string(roi-line-cost grand-total-line))
             (real->decimal-string(roi-line-value grand-total-line))
             (real->decimal-string(roi-line-gain-loss grand-total-line))
             (real->decimal-string(roi-line-roi grand-total-line))))))      
      hash-result))
