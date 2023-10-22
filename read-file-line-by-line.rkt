#lang racket
(require racket/block)

#|
Parse a text file with sections (like an XML file read as normal text)

INPUT
-----
some text
some text
START1
section 1 1
section 1 2
END1
some text
some text
START2
section 2 1
section 2 2
END2

OUTPUT
------
((section1 ("section 1 1" "section 1 2")) (section2 ("section 2 1" "section 2 2")))


|#

(define sample-file "D:\\__DATA_FOR_APPS\\GnuCash-Uncompressed\\conjoint-UNCOMPRESSED-SNAPSHOT.gnucash")

(define (all-lines path)
   (call-with-input-file path    
    (lambda (in)
      (let loop ([line (read-line in 'any)])
        (if (eof-object? line)
            void
            (block
             (process-line line)
             (loop (read-line in 'any))))))))
;;      (displayln (read-line in)))))

(define (process-line line)
  (if (string-prefix? line "THIS")
      (displayln "THIS IS IT")
      (displayln line)))


;; given current-input set to the desired file
;; loop over the lines
(define (loop-input in)
  (let loop ([line (read-line in 'any)])
    (if (eof-object? line)
        void
        (block
         (displayln line)
         (loop (read-line in 'any))))))

(all-lines sample-file)

#|  
(define (test-read)
  (with-input-from-file sample-file
    (lambda () (read-line (current-input-port) 'any))))

(test-read)
|#

;;
;; VERSION 1: generalize reading a section 
;; 


;; line file sectiont-tag start-tag end-tag -> (section-tag (.. lines))

#|

(define (read-to-end-of-file file)
  (let ((line (read-line file 'any)))
    (unless (eof-object? line)
      (if (string-prefix? line "THIS")
          (read-to-end-of-file line file)
          (displayln line))
      (read-to-end-of-file  file))))

(define (line file sectiont-tag start-tag end-tag)
  (define (loop-till-end-tag file end-tag list-lines)
    (let ((line (read-line file 'any)))
      (cond
        [(or (equal? line end-tag) (eof-object? line))
|#       
         
             
        




#|

(define (read-until-THAT line file)
  (unless (eof-object? line)
    (printf "THIS line is special: ~a" line))
   )


(define (next-line-it file)
  (let ((line (read-line file 'any)))
    (unless (eof-object? line)
      (if (string-prefix? line "THIS")
          (read-to-end-of-file line file)
          (displayln line))
      (next-line-it file))))

(call-with-input-file sample-file next-line-it)

|#