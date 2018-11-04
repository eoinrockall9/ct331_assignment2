#lang racket

(provide ins_beg)
(provide ins_end)
(provide cout_top_level)


(define (ins_beg el lst)
  (cons el lst))

(define (ins_end el lst)
  (append lst (list el)))

(define (cout_top_level lst)
  (length (flatten lst)))


(display "Tests for ins_beg:\n")

(ins_beg 'a '(b c d))
(ins_beg '(a b) '(b c d))

(display "Test for ins_end:\n")
(ins_end 'a '(b c d))
(ins_end '(a b) '(b c d))

(display "Test for cout_top_level:\n")
(cout_top_level '(a b c (d e) (f g)))





