;---- Logging Functions
(define (debug-log message)
    (display "DEBUG: ")
    (display message)
    (newline)
)
;TODO Remember to remove all debug-log from functions before submission

(define (error-log message)
    (display "ERROR: ")
    (display message)
    (newline)
)


; nth-item - get's the nth item from a list
; ASSUMPTION: List is not empty
(define (nth-item n alist)
    (if (= n 1) (car alist) (nth-item (- n 1) (cdr alist))) 
)


; replace-nth-item - replaces the nth item from a list with a given value
; ASSUMPTION: List is not empty, n is <= length of list
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

(define (swap-element n1 n2 alist)
    (let ((temp (nth-item n1 alist)))
        (replace-nth-item n2 (replace-nth-item n1 alist (nth-item n2 alist)) temp)
    )
)