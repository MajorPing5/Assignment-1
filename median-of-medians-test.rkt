#lang racket

(require rackunit)                     ; Import the rackunit library for testing
(require "median-of-medians.rkt")      ; Import the functions from your median-of-medians file

;; Test for the `list-has-five-or-fewer?` function
(define (test-list-has-five-or-fewer?)
  (check-equal? (list-has-five-or-fewer? '()) #t)                ; Empty list should return #t
  (check-equal? (list-has-five-or-fewer? '(1)) #t)               ; Length 1 list should return #t
  (check-equal? (list-has-five-or-fewer? '(1 2)) #t)             ; Length 2 list should return #t
  (check-equal? (list-has-five-or-fewer? '(1 2 3)) #t)           ; Length 3 list should return #t
  (check-equal? (list-has-five-or-fewer? '(1 2 3 4)) #t)         ; Length 4 list should return #t
  (check-equal? (list-has-five-or-fewer? '(1 2 3 4 5)) #t)       ; Length 5 list should return #t
  (check-equal? (list-has-five-or-fewer? '(1 2 3 4 5 6)) #f))    ; Length 6 list should return #f

;; Test for the `median-of-medians` function
(define (test-median-of-medians)
  (check-equal? (median-of-medians '()) '())                   ; Length 0 list should return '()
  (check-equal? (median-of-medians '(1 2 3 4 5 6))
                (median-of-medians '(1 2 3 4 5) '(6))))         ; Length 6 list should be split

;; Run all tests
(test-list-has-five-or-fewer?)
(test-median-of-medians)
