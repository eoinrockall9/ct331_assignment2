#lang racket

(provide ins_beg)
(provide ins_end)
(provide count_top_level)
(provide count_instances)
(provide count_instances_tr)
(provide count_instances_deep)

(define (ins_beg el lst)
  (cons el lst))

(define (ins_end el lst)
  (append lst (list el)))

(define (count_top_level lst)
  (length (flatten lst)))

(define (count_instances el lst)
  (if (null? lst)
      0
      (if (equal? el (car lst))
          (+ 1 (count_instances el (cdr lst)))
          (count_instances el (cdr lst))
      )
  )
)

(define (tail el lst)
  (count_instances_tr el lst 0))

(define (count_instances_tr el lst)
  (count_instances_tr el lst 0))
(define (count_instances_tr_total el lst total) 
  (cond [(empty? lst) total]
    [(equal? (car lst) el) (count_instances_tr el (cdr lst) (+ 1 total))]
    [else (count_instances_tr el (cdr lst) total)]))

(define (count_instances_deep el lst)
  (cond ((null? lst) 0)
        ((equal? el (car lst))
         (+ 1 (count_instances_deep el (cdr lst))))
        ((list? (car lst))
         (+ 0 (count_instances_deep el (cdr lst))
             (count_instances_deep el (car lst))))
        (else (count_instances_deep el (cdr lst)))
))

(display "Tests for ins_beg:\n")

(ins_beg 'a '(b c d))
(ins_beg '(a b) '(b c d))

(display "Test for ins_end:\n")
(ins_end 'a '(b c d))
(ins_end '(a b) '(b c d))

(display "Test for count_top_level:\n")
(count_top_level '(a b c (d e) (f g)))

(display "Test for count_instances:\n")
(count_instances 'a '(a b c d a f a))

(display "Test for count_instances_tr:\n")
(count_instances_tr 'a '(a b c d a f a))

(display "Test for count_instances_deep:\n")
(count_instances_deep 'a '( a b c '(a b a) c))





