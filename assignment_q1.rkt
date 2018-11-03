#lang racket

(cons 1 2)

(cons 1(cons 2 (cons 3 empty)))

(cons "banana" (cons 1 (cons '(1 2 3) empty)))

'("banana" 1 (1 2 3))

(append '("banana") '(1) '(1 2 3))