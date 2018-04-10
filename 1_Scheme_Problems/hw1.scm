
;Find Biggest - Create a function to find the maximum of a list 
(define (max x y)
    (if (> x y) x y))

(define (find-biggest alist)
    (if (null? (cdr alist)) (car alist) (max (car alist) (find-biggest (cdr alist)))
    ))


; count-from - Inclusively displays all the numbers between A and B
(define (count-from A B)
    (cond
        [(<= A B) 
            (display A)
            (newline)
            (count-from (+ A 1) B)
        ]
    )
)

;nth-item