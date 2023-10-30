#lang racket/gui
(require "gnucash-parser.rkt")

(define SMALL-SAMPLE-GNUCASH-FILE
  "D:\\__DATA_FOR_APPS\\GnuCash-Uncompressed\\conjoint-UNCOMPRESSED-SNAPSHOT.gnucash")


;; ------------
;; --  MAIN  --
;; ------------
(define (main-gnucash)
  (import-gnucash-file SMALL-SAMPLE-GNUCASH-FILE)
  (print-overview))

(main-gnucash)