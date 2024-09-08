#lang racket

;; Helper function: Insertion sort for small lists of size <= 5
(define (insertion-sort number-list)
  (if (null? number-list)
      '()
      (let insert ((current-number (car number-list))
                   (sorted-list (insertion-sort (cdr number-list))))
        (if (null? sorted-list)
            (list current-number)
            (if (<= current-number (car sorted-list))
                (cons current-number sorted-list)
                (cons (car sorted-list) (insert current-number (cdr sorted-list))))))))

;; Helper function: Get the nth element of a list
(define (get-nth-element list n)
  (if (zero? n)
      (car list)
      (get-nth-element (cdr list) (- n 1))))

;; Helper function: Find the median of a list of up to 5 elements
(define (find-median small-list)
  (let* ((sorted-list (insertion-sort small-list))
         (list-length (let count-elements ((current-list sorted-list) (count 0))
                        (if (null? current-list)
                            count
                            (count-elements (cdr current-list) (+ count 1))))))
    (cond
      [(= list-length 0) '()]
      [(= list-length 1) (get-nth-element sorted-list 0)]
      [(= list-length 2) (get-nth-element sorted-list 1)]
      [(= list-length 3) (get-nth-element sorted-list 1)]
      [(= list-length 4) (get-nth-element sorted-list 2)]
      [else (get-nth-element sorted-list 2)]))) ; For a list of exactly 5 elements

;; Helper function: Split a list into groups of up to 5 elements
(define (split-into-groups-of-five number-list)
  (if (null? number-list)
      '()
      (cons (take number-list 5)
            (split-into-groups-of-five (drop number-list 5)))))

;; Helper function: Median of Medians pivot selection
(define (median-of-medians number-list)
  (let* ((groups-of-five (split-into-groups-of-five number-list))
         (medians (map find-median groups-of-five)))
    (with-handlers ([exn:fail? (lambda (e) (find-median medians))])
      (if (null? (fifth medians))  ; Check if there are fewer than 5 medians
          (find-median medians)
          (median-of-medians medians)))))

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

;; Quicksort using Median-of-Medians, with error handling for list length
(define (quicksort number-list)
  (with-handlers ([exn:fail? (lambda (e) (insertion-sort number-list))])
    (if (null? (fifth number-list))  ; Check if there are fewer than 5 elements
        (insertion-sort number-list)
        (let* ((pivot (median-of-medians number-list)))
          (call-with-values
            (lambda () (partition-list-around-pivot number-list pivot))
            (lambda (less-than-pivot greater-than-pivot)
              (append (quicksort less-than-pivot)
                      (list pivot)
                      (quicksort greater-than-pivot))))))))

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
    (displayln "Generated List:")  ; Display the generated list
    (displayln unsorted-list)      ; Print the generated list
    (let ((sorted-list (quicksort unsorted-list)))
      (displayln "Sorted List:")   ; Display the sorted list
      (displayln sorted-list))))   ; Print the sorted list
