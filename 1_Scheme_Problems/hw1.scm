
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



; sorted?
(define (compare-first-two-from-list alist)
    (< (car alist) (car (cdr alist)))
)

(define (sorted? alist)
    (cond 
        [(null? (cdr (cdr alist)))
         (compare-first-two-from-list alist)
        ]
        [#t  
            (and (compare-first-two-from-list alist) (sorted? (cdr alist)))
        ]
        
    )
)

;apply-action
;Takes two parameters: an action and a state. Returns the updated state that will result by following that action.
;A state is a list of three elements: an X position, a Y position, and a direction (N, S, E, or W). An action is one
;of the following strings: STAY, MOVE-1, MOVE-2, MOVE-3, TURN-LEFT, TURN-RIGHT, or TURN-
;AROUND. Assume that all moves are forward, relative to the current direction. For example:

;input  (apply-action ‘(5 2 N) “MOVE-2”)
;output (5 4 N)

(define (debug-log message)
    (display "DEBUG: ")
    (display message)
    (newline)
)

(define (error-log message)
    (display "ERROR: ")
    (display message)
    (newline)
)

(define (apply-action-move state move-val)
    (debug-log move-val)
)

(define (apply-action-turn state turn-val)
    (debug-log turn-val)
)

(define (apply-action state action)
    (debug-log (substring action 0 4))
    (cond 
        [ (equal? (substring action 0 4) "STAY")
            state
        ]
        [ (equal? (substring action 0 4) "MOVE")
            (apply-action-move state (substring action 5 (string-length action)))
        ]
        [ (equal? (substring action 0 4) "TURN")
            (apply-action-turn state (substring action 5 (string-length action)))
        ]
        [#t  
          (error-log "Invalid Move")
        ]
        
    )
)

;(apply-action '(0 0 S) "TURN-RIGHT")