;input data 
(define adjacency-map '(
    (Alabama Mississippi Tennessee Georgia Florida)
    (Alaska)
    (Arkansas Texas Oklahoma Missouri Tennessee Mississippi Louisiana)
    (Arizona California Nevada Utah New-Mexico)
    (California Arizona Nevada Oregon)
    (Colorado New-Mexico Utah Wyoming Nebraska Kansas Oklahoma)
    (Connecticut New-York Massachusetts Rhode-Island)
    (Delaware Maryland Pennsylvania New-Jersey)
    (Florida Alabama Georgia)
    (Georgia Florida Alabama Tennessee North-Carolina South-Carolina)
    (Hawaii)
    (Idaho Oregon Washington Montana Wyoming Utah Nevada)
    (Indiana Illinois Michigan Ohio Kentucky)
    (Illinois Missouri Iowa Wisconsin Indiana Kentucky)
    (Iowa Missouri Illinois Wisconsin Minnesota South-Dakota Nebraska)
    (Kansas Colorado Nebraska Missouri Oklahoma)
    (Kentucky Missouri Illinois Indiana Ohio West-Virginia Virginia Tennessee)
    (Louisiana Texas Arkansas Mississippi)
    (Maine New-Hampshire)
    (Maryland Virginia West-Virginia Pennsylvania Delaware)
    (Massachusetts Rhode-Island Connecticut New-York Vermont New-Hampshire)
    (Michigan Wisconsin Indiana Ohio)
    (Minnesota North-Dakota South-Dakota Iowa Wisconsin)
    (Mississippi Louisiana Arkansas Tennessee Alabama)
    (Missouri Oklahoma Kansas Nebraska Iowa Illinois Kentucky Tennessee Arkansas)
    (Montana Idaho Wyoming South-Dakota North-Dakota)
    (Nebraska Colorado Kansas Missouri Iowa South-Dakota Wyoming)
    (Nevada California Arizona Utah Idaho Oregon)
    (New-Hampshire Maine Vermont Massachusetts)
    (New-Jersey Delaware Pennsylvania New-York)
    (New-Mexico Texas Oklahoma Colorado Arizona)
    (New-York Pennsylvania New-Jersey Connecticut Massachusetts Vermont)
    (North-Carolina South-Carolina Georgia Tennessee Virginia)
    (North-Dakota Montana South-Dakota Minnesota)
    (Ohio Michigan Indiana Kentucky West-Virginia Pennsylvania)
    (Oklahoma Texas New-Mexico Colorado Kansas Missouri Arkansas)
    (Oregon Washington Idaho Nevada California)
    (Pennsylvania Ohio West-Virginia Maryland Delaware New-Jersey New-York)
    (Rhode-Island Connecticut Massachusetts)
    (South-Carolina Georgia North-Carolina)
    (South-Dakota Nebraska Iowa Minnesota North-Dakota Montana Wyoming)
    (Tennessee Arkansas Missouri Kentucky Virginia North-Carolina Georgia Alabama Mississippi)
    (Texas New-Mexico Oklahoma Arkansas Louisiana)
    (Utah Nevada Idaho Wyoming Colorado Arizona)
    (Vermont New-York Massachusetts New-Hampshire)
    (Virginia North-Carolina Tennessee Kentucky West-Virginia Maryland)
    (Washington Oregon Idaho)
    (West-Virginia Virginia Kentucky Ohio Pennsylvania Maryland)
    (Wisconsin Minnesota Iowa Illinois Michigan)
    (Wyoming Idaho Montana South-Dakota Nebraska Colorado Utah)
  )
)

;---- Logging Functions
(define (log-helper message-list)
    (cond   
        [ (null? message-list)
                (newline)
        ]
        [ #t 
            (display (car message-list))
            (log-helper (cdr message-list))
        ]
    )
)
; TODO This function doesn't print (1 2 $3) with the value of $3

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

(define (index-of-helper state alist counter)
    (cond 
        [
            (null? alist) 
                (error-log (cons "could not find state" state) )
                #f
        ]
        [
            (equal? (car (car alist)) state)
                counter
        ]
        [
            #t 
                (index-of-helper state (cdr alist) (+ counter 1))
        ]
    )
)

;(index-of 'Hawaii adjacency-map)
(define (index-of state alist)
    (index-of-helper state alist 1)
)

(define (pop-list alist)
    (reverse (cdr (reverse alist)))
)
;(contains 'A '(A B))
; #t
;(contains 'A '(B C))
; #f
(define (contains state alist)
    (cond 
        [
            (null? alist) 
                #f
        ]
        [
            (equal? (car alist) state)
                #t
        ]
        [
            #t 
                (contains state (cdr alist))
        ]
    )
)

;(is-adjacent 'New-York 'Vermont)
; #t
;(is-adjacent 'New-York 'Florida)
; #f
(define (is-adjacent state1 state2)
    (contains state2 (nth-item (index-of state1 adjacency-map) adjacency-map))
)

(define (list-length alist)
    (if (null? alist) 0 (+ 1 (list-length(cdr alist))))
)

; (possible-swaps 5 1 2 '())
; ((4 5) (3 5) (3 4) (2 5) (2 4) (2 3) (1 5) (1 4) (1 3) (1 2))
(define (possible-swaps-helper list-length swap1 swap2 swap-list)
    (cond
        [
            (equal? swap2 list-length)
                (possible-swaps-helper list-length (+ swap1 1)  (+ swap1 2) (cons (list swap1 swap2) swap-list ))
        ]
        [
            (> swap2 list-length)
                swap-list
        ]
        [
            #t
                (possible-swaps-helper list-length swap1  (+ swap2 1) (cons (list swap1 swap2) swap-list ))
        ]
    )
)

; (possible-swaps 5)
; (4 5) (3 5) (3 4) (2 5) (2 4) (2 3) (1 5) (1 4) (1 3) (1 2))
(define (possible-swaps list-length)
    (possible-swaps-helper list-length 1 2 '())
)

; TODO DON'T FORGET - LENGTH >=2. IF LENGTH <=1. IT IS TRUE !!!!

(define (get-children-helper state-list swap-list children-list swap-state)
    (cond
        [
            (null? swap-list)
                children-list
        ]
        [
            #t
                (get-children-helper state-list (cdr swap-list) (cons (cons (swap-element (nth-item 1 (car swap-list)) (nth-item 2 (car swap-list)) state-list)  (list(cons (car swap-list) swap-state))) children-list) swap-state)
        ]
    )
)

(define (get-children state-list result swap-state)
    (get-children-helper state-list (possible-swaps (list-length state-list)) result swap-state)
)
;(get-children '(A B C D) '() '(1 2)')
;(((B A C D) 1 2) ((C B A D) 1 3) ((D B C A) 1 4) ((A C B D) 2 3) ((A D C B) 2 4) ((A B D C) 3 4))

;(is-goal-state '((California Washington Oregon) ()))
;
(define (is-goal-state-helper state-list)
    ;(display "is-goal-state-helper: ") (display state-list) (newline)
    (cond
        [
            (null? (cdr state-list))
                #t
        ]
        [
            #t
                ;(display "Comparing ") (display (car state-list)) (display " and ") (display (car (cdr state-list))) (newline)
                (if (is-adjacent (car state-list) (car (cdr state-list)))
                    (is-goal-state-helper (cdr state-list))
                    #f
                )
        ]
    )
)

(define (is-goal-state child)
    ;(display "is-goal-state: ") (display child) (newline)
    (is-goal-state-helper (car child))
)

(define (ldfs-helper frontier max-depth counter state-history)
    ;(display "Frontier: ") (display frontier) (newline)
    (cond 
        [(null? frontier)   ;Base Care : if you have a null list, you failed
            #f 
        ]
        [
            (equal? (car frontier) 'MARKER)
            (display "<<<<< POP UP") (newline)
            (ldfs-helper  (cdr frontier) max-depth (- counter 1) state-history)
        ]
        [(is-goal-state (car frontier) ) ; Check the head of the frontier if it's the goal state
            (display "FOUND GOAL STATE: ")
            (display (car frontier)) (newline)
            (display "HERE") (newline)
            (list (car (car frontier)) (reverse (car(cdr (car frontier)))))
        ]
        [
            (contains (car (car frontier)) state-history )
            (display "===== HISTORY") (newline)
                ;(display "Counter: ") (display counter) (newline)
                (newline)
                (display "Car: ") (display (car frontier)) (newline)
                (ldfs-helper  (cdr frontier) max-depth counter state-history )
        ]
        [
            (>= counter max-depth)
                (newline)
                (display "<<<<< SAME LEVEL") (newline)
                (display "Car: ") (display (car frontier)) (newline)
                (ldfs-helper  (cdr frontier) max-depth counter (cons (car (car frontier)) state-history) )
        ]
        [ #t               ; append children of frontier with the cdr of the frontier back into dfs
            (newline)
            (display ">>>>> GO DOWN ") (newline)
            (display "Car: ") (display (car frontier)) (newline)
            (if (= counter max-depth)
                (ldfs-helper  (cdr frontier) max-depth counter (cons (car (car frontier)) state-history))
                (ldfs-helper (append (append (get-children (car (car frontier)) '(MARKER) (car(cdr (car frontier)))) ) (cdr frontier)) max-depth (+ counter 1) (cons (car (car frontier)) state-history) )
                
            )
                ;(append (get-children (car (car frontier)) '()) (cdr frontier))
        ]
    )
)


;(ldfs '(California Washington Oregon Arizona Montana Utah) 4)
;(ldfs '(California Washington Idaho Arizona Oregon Montana) 4)
;(ldfs'(California Washington Oregon Arizona Utah Idaho Montana Florida) 2)
;(ldfs '(California Washington Idaho Arizona Oregon Montana) 5)
(define (ldfs state-list max-depth)
    ;TODO CHECK length=1 and length=0 case
    (ldfs-helper (get-children state-list '() '()) max-depth 1 (list state-list))
)

(define (id-dfs-helper state-list max counter)
    (display "Hello World")
)

(define (id-dfs state-list)
    (display "Hello World")
)

(define (permutation number)
    (if (= number 1) 
        number
        (* number (permutation (- number 1)))    
    )
)

