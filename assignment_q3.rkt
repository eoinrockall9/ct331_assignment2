#lang racket

(provide create)
(provide order)
(provide right_tree)
(provide value)
(provide left_tree)
(provide check)
(provide is_empty)
(provide insert)
(provide insert_list)
(provide sort)

;helper function 
(define (create left-sub val right-sub)
  (list left-sub val right-sub))

;helper function
(define (right_tree bst) (caddr bst))

;helper function
(define (value bst) (cadr bst))

;helper function
(define (left_tree bst) (car bst))

;helper function
(define (is_empty bst)
  (null? bst))

;A
(define (order bst)
  (cond ((is_empty bst))
        (else
         (order (left_tree bst))
         (display (value bst)) (newline)
         (order (right_tree bst)))))
;B
(define (check item bst)
  (cond ((is_empty bst) #f)
        ((equal? item (value bst)) #t)
        ((< item (value bst))
         (check item (left_tree bst)))
        ((> item (value bst))
         (check item (right_tree bst)))
        (else bst)))

;C
(define (insert item bst)
  (cond ((is_empty bst)
         (create '() item '()))
        ((< item (value bst)) ;if item less than root, add to left sub_tree
         (create (value bst)
                 (insert item (left_tree bst))
                 (right_tree bst)))
        ((> item (value bst)) ;if item greater than root, add to right sub_tree
         (create (value bst)
                 (left_tree bst)
                 (insert item (right_tree bst))))
        (else bst)))

;D
(define (insert_list lst bst)
  (for ([item 'lst]) ; loop through given list and for each element in list, use insert function to add them to tree
    insert item bst
    )
)

;E
(define (tree-sort tree)
  (if (empty? tree)
      '()
      (append (tree-sort (left_tree tree))
              (list (value tree))
              (tree-sort (right_tree tree)))
  )
)

;F
(define (treesort_compare lst comparator)
  (inorder_to_list (insert_list_compare '() lst comparator)))
(define (insert_list_compare tree lst comparator)
  (if (empty? lst)
     tree
     (insert_list_compare (insert_item_compare tree (car lst) comparator) (cdr lst) comparator)))
(define (insert_item_compare tree item comparator)
  (cond
    [(null? tree) (list '() item '())]
    [(equal? item (cadr tree)) tree]
    [(comparator item (cadr tree)) (list (insert_item_compare (car tree) item comparator) (cadr tree) (caddr tree))]
    [else (list (car tree) (cadr tree) (insert_item_compare (caddr tree) item comparator))]))