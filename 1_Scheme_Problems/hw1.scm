
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
(define (nth-item n alist)
    (if (= n 1) (car alist) (nth-item (- n 1) (cdr alist))) 
)


;replace-nth-item
; DOESN'T WORK
(define (replace-nth-item-helper counter n val in_list )
    (cond   
        [ (= n counter) 
            (replace-nth-item-helper (+ counter 1) n val (cdr in_list) (cons out_list val))
        ]
        [ (null? in_list) 
            out_list
        ]
        [ #t 
            (replace-nth-item-helper (+ counter 1) n val (cdr in_list) (cons out_list (car in_list)))
        ]
    )
)

(define (replace-nth-item n alist val)
    (cond   
        [ (null? alist)
            '()
        ]
        [ (= n 1) 
        (cons val (replace-nth-item (- n 1) (cdr alist) val))
        ]
        [ #t 
            (cons (car alist) (replace-nth-item (- n 1) (cdr alist) val))
        ]
    )
)
