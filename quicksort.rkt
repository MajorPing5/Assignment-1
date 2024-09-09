#lang racket

;; Provide the `quicksort` function for use in other modules
(provide quicksort)

;; Import the median-of-medians functions
(require "median-of-medians.rkt")

;; Helper function: Partition the list around the pivot
(define (partition-list-around-pivot number-list pivot)
  (let partition-helper ((current-list number-list)
                         (less-than-pivot '())
                         (greater-than-pivot '()))
    (cond
      [(null? current-list) (values less-than-pivot greater-than-pivot)]
      [(<= (car current-list) pivot) 
       (partition-helper (cdr current-list) (cons (car current-list) less-than-pivot) greater-than-pivot)]
      [else 
       (partition-helper (cdr current-list) less-than-pivot (cons (car current-list) greater-than-pivot))])))

(define (quicksort number-list)
  (if (list-has-five-or-fewer? number-list)
      (insertion-sort number-list)
      (let* ((pivot (median-of-medians number-list)))
        (call-with-values
          (lambda () (partition-list-around-pivot number-list pivot))
          (lambda (less-than-pivot greater-than-pivot)
            (append (quicksort less-than-pivot)
                    (list pivot)
                    (quicksort greater-than-pivot)))))))