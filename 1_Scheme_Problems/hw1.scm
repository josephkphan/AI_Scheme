; Creator: Joseph Phan
; Homework 1: Scheming
; Coen266 
; Spring 2018


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

; ------ Basic Assignment: 

; Find Biggest - Create a function to find the maximum of a list 
; ASSUMPTION: List is not empty
(define (max x y)
    (if (> x y) x y))

(define (find-biggest alist)
    (if (null? (cdr alist)) (car alist) (max (car alist) (find-biggest (cdr alist)))
    ))

; count-from - Inclusively displays all the numbers between A and B
; ASSUMPTION: A < B
(define (count-from A B)
    (cond
        [(<= A B) 
            (display A)
            (newline)
            (count-from (+ A 1) B)
        ]
    )
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



; sorted? - returns boolean value if a list is sorted or not
; ASSUMPTION: List is not empty
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

(define (apply-action-move state move-val)
    (cond 
        [ (equal? (nth-item 3 state) 'N)
                (replace-nth-item 2 state (+ (nth-item 2 state) (string->number move-val)))
        ]
        [ 
            (equal? (nth-item 3 state) 'W)
                (replace-nth-item 1 state (- (nth-item 1 state) (string->number move-val)))
        ]
        [ 
            (equal? (nth-item 3 state) 'S)
                (replace-nth-item 2 state (- (nth-item 2 state) (string->number move-val))) 
        ]
        [ 
            (equal? (nth-item 3 state) 'E)
                (replace-nth-item 1 state (+ (nth-item 1 state) (string->number move-val))) 
        ]
        [#t  
          (error-log "Invalid Direction Given")
        ]
    )
)

(define (apply-action-turn state turn-val)
    (cond 
        [ (equal? (nth-item 3 state) 'N)
            (cond
                [
                    (equal? turn-val "LEFT")
                        (replace-nth-item 3 state 'W)
                ]
                [
                    (equal? turn-val "RIGHT")
                    (replace-nth-item 3 state 'E)
                ]
                [
                    (equal? turn-val "AROUND")
                    (replace-nth-item 3 state 'S)
                ]    
            )            
        ]
        [ (equal? (nth-item 3 state) 'W)
            (cond
                [
                    (equal? turn-val "LEFT")
                        (replace-nth-item 3 state 'S)
                ]
                [
                    (equal? turn-val "RIGHT")
                    (replace-nth-item 3 state 'N)
                ]
                [
                    (equal? turn-val "AROUND")
                    (replace-nth-item 3 state 'E)
                ]    
            )
        ]
        [ (equal? (nth-item 3 state) 'S)
            (cond
                [
                    (equal? turn-val "LEFT")
                        (replace-nth-item 3 state 'E)
                ]
                [
                    (equal? turn-val "RIGHT")
                    (replace-nth-item 3 state 'W)
                ]
                [
                    (equal? turn-val "AROUND")
                    (replace-nth-item 3 state 'N)
                ]    
            )
        ]
        [ (equal? (nth-item 3 state) 'E)
            (cond
                [
                    (equal? turn-val "LEFT")
                        (replace-nth-item 3 state 'N)
                ]
                [
                    (equal? turn-val "RIGHT")
                    (replace-nth-item 3 state 'S)
                ]
                [
                    (equal? turn-val "AROUND")
                    (replace-nth-item 3 state 'W)
                ]    
            )
        ]
        [#t  
          (error-log "Invalid Direction Given")
        ]
    )
)

(define (apply-action state action)
    (cond 
        [ 
            (equal? (substring action 0 4) "STAY")
                state
        ]
        [ 
            (equal? (substring action 0 4) "MOVE")
                (apply-action-move state (substring action 5 (string-length action)))
        ]
        [ 
            (equal? (substring action 0 4) "TURN")
                (apply-action-turn state (substring action 5 (string-length action)))
        ]
        [
            #t  
                (error-log "Invalid Move")
        ]
        
    )
)


; ------ Advanced Assignment: 

; get-location - gets the location of a given coordinate given the percept
; ASSUMPTION - percept remains the same structure as given in the example (Triangle shaped from origin)
(define (get-x-row agent-world yCoord)
    (nth-item yCoord agent-world)
)

(define (translate-xCoord-to-index xCoord yCoord)
    (+ (+ xCoord yCoord) 1)
)

(define (get-location agent-world xCoord yCoord)
    (nth-item (translate-xCoord-to-index xCoord yCoord) (get-x-row agent-world yCoord))
)
 