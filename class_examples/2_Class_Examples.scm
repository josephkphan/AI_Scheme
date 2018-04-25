; goal-test - if the specified state is the goal
; get-children - get us a list of all the states of a children from a given state

(define (dfs frontier)
    
    (cond 
        ((null? frontier)   ;Base Care : if you have a null list, you failed
            #f )
        ((goal-test (car frontier) ) ; Check the head of the frontier if it's the goal state
            (car frontier))
        ( #t               ; append children of frontier with the cdr of the frontier back into dfs
            (dfs (append (get-children car(frontier)) (cdr frontier)))
    )
)


; Using Let - create a temporary variable (let are never necessary - just to be clean)
(define (dfs frontier)
    (cond 
        (#t 
            (let ((candidate (car frontier)))
                (cond 
                    ((goal-test candidate) candidate)
                    (#t (dfs (append (get-children candidate) (cdr frontier))))
                    
                )
            )
        )
    )
)



((large small) () () )
(1 1) ; where it is position of (small disk, large disk)
((small 1)(large 1))