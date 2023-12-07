#lang racket

(require racket/runtime-path)

(provide (all-defined-out))

;; module to hold racket code defining settings
(define TEST-GNUCASH-FILE-1 "d:\\Documents\\programming\\racket\\racket-projects\\GnuCash\\gnucash-web-app\\tests\\test-file1.gnucash")

;; run this to define the full path to allocation file in the programs folder
;; can only run at top-level
(define-runtime-path allocation-file-path "allocation-data.txt")






