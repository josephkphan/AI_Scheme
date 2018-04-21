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

(define (index-of state alist)
    (index-of-helper state alist 1)
)
;(index-of 'Hawaii adjacency-map)

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
;scheme@(guile-user) [15]> (contains 'A '(A B))
;$16 = #t
;scheme@(guile-user) [15]> (contains 'A '(B C))
;$17 = #f

(define (is-adjacent state1 state2)
    (contains state2 (nth-item (index-of state1 adjacency-map) adjacency-map))
)
;scheme@(guile-user) [16]> (is-adjacent 'New-York 'Vermont)
;$21 = #t
;scheme@(guile-user) [16]> (is-adjacent 'New-York 'Florida)
;$22 = #f


(define (list-length alist)
    (if (null? alist) 0 (+ 1 (list-length(cdr alist))))
)

; scheme@(guile-user) [14]> (possible-swaps 5 1 2 '())
; $48 = ((4 5) (3 5) (3 4) (2 5) (2 4) (2 3) (1 5) (1 4) (1 3) (1 2))
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

; scheme@(guile-user) [17]> (possible-swaps 5)
; $49 = ((4 5) (3 5) (3 4) (2 5) (2 4) (2 3) (1 5) (1 4) (1 3) (1 2))
(define (possible-swaps list-length)
    (possible-swaps-helper list-length 1 2 '())
)

; TODO DON'T FORGET - LENGTH >=2. IF LENGTH <=1. IT IS TRUE !!!!


;(get-children '(A B C)  '()')

;scheme@(guile-user) [2]> (list '(A B C) '( 1 2 ) )
;$4 = ((A B C) (1 2))


(define (get-children-helper states swap-list children-list)
    (cond
        [
            (null? swap-list)
                children-list
        ]
        [
            #t
                (get-children-helper states (cdr swap-list) (cons (cons (swap-element (nth-item 1 (car swap-list)) (nth-item 2 (car swap-list)) states)  (car swap-list)) children-list))
        ]
    )
)

(define (get-children states)
    (get-children-helper states (possible-swaps (list-length states)) '())
)


