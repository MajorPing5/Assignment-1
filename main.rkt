#lang racket
(require "quicksort.rkt")
(require "median-of-medians.rkt")

;; Random Number ganerator
(define (generate-random-integers count min-value max-value)
 (define (generate n)
   (if (zero? n)
       '()
       (cons (random min-value max-value)
             (generate (- n 1)))))
  (generate count))

;; Function to display the generated list and then sort it
(define (display-and-quicksort-random-list count min-value max-value)
  (let ((unsorted-list
         (generate-random-integers count min-value max-value)))
    #| (displayln "Generated List:")  ; Display the generated list
    (displayln unsorted-list)      ; Print the generated list |#
    (let ((sorted-list (quicksort unsorted-list)))
      (displayln "Sorted List:")   ; Display the sorted list
      (displayln sorted-list))))   ; Print the sorted list
