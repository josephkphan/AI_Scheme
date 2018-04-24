; Author:       Joseph Phan
; Class:        Coen 266 AI
; Assignment 2: Searching 
; Date:         Spring 2018
; ------------------------------- Input Data -----------------------------------

; Input Data Definition of US Map and the adjacency relationship across states
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

; ------------------------------- Logging Functions -----------------------------------

; Used as logging function for troubleshooting
; TODO Remember to remove all debug-log from functions before submission
(define (debug-log message)
    (display "DEBUG: ")
    (display message)
    (newline)
)

; Used for error messages
(define (error-log message)
    (display "ERROR: ")
    (display message)
    (newline)
)

; ------------------------------- Simple Functions -----------------------------------

; factorial: Provides the factorial mathematical result of the given number
; Assumption: Input >= 1
; Example Input: (factorial 5)
; Example Output: 120
(define (factorial number)
    (if (<= number 1) 
        number
        (* number (factorial (- number 1)))    
    )
)


; nth-item: get's the nth item from a list
; ASSUMPTION: List is not empty
; Example Input: (nth-item 3 '(A B C))
; Example Output:  C
(define (nth-item n alist)
    (if (= n 1) (car alist) (nth-item (- n 1) (cdr alist))) 
)


; replace-nth-item: replaces the nth item from a list with a given value
; ASSUMPTION: List is not empty, n is <= length of list
; Example Input: (replace-nth-item 3 '(A B C) 'D)
; Example Output:(A B D)
(define (replace-nth-item n alist val)
    (cond   
        ( (null? alist)
            '()
        )
        ( (= n 1) 
        (cons val (replace-nth-item (- n 1) (cdr alist) val))
        )
        ( #t 
            (cons (car alist) (replace-nth-item (- n 1) (cdr alist) val))
        )
    )
)

; swap-element: swaps two elements in a list
; ASSUMPTION: n1 and n2 are valid inputs in the list
; Example Input: (swap-element 2 3 '(A B C))
; Example Output: (A C B)
(define (swap-element n1 n2 alist)
    (let ((temp (nth-item n1 alist)))
        (replace-nth-item n2 (replace-nth-item n1 alist (nth-item n2 alist)) temp)
    )
)

; pop-list: Used to remove the last element of a list
; Assumption: List is not empty
; Example Input: (pop-list '(A B C))
; Example Output: (A B)
(define (pop-list alist)
    (reverse (cdr (reverse alist)))
)

; contains: see if an item is in a list 
; Assumption: Not a nested List
; Example Input:(contains 'A '(A B))
; Example Output: #t
(define (contains item alist)
    (cond 
        (
            (null? alist) 
                #f
        )
        (
            (equal? (car alist) item)
                #t
        )
        (
            #t 
                (contains item (cdr alist))
        )
    )
)

; list-length: outputs the length of a given list
; Example Input: (list-length '(A B C))
; Example Output: 3
(define (list-length alist)
    (if (null? alist) 0 (+ 1 (list-length(cdr alist))))
)

; --------------------------- US States Specific Functions -------------------------------

; helper function for index-of
(define (index-of-helper state alist counter)
    (cond 
        (
            (null? alist) 
                (error-log (cons "could not find state" state) )
                #f
        )
        (
            (equal? (car (car alist)) state)
                counter
        )
        (
            #t 
                (index-of-helper state (cdr alist) (+ counter 1))
        )
    )
)

; index-of: Finds the index of a state in the defined adjacency map
; Example Input: (index-of 'Hawaii adjacency-map)
; Example Output: 11
; Error Handling: will print out error message is state is not found in adjacency map
(define (index-of state alist)
    (index-of-helper state alist 1)
)


; is-adjacent: returns a boolean of whether two states are adjacent or not
; Example Input: (is-adjacent 'New-York 'Vermont)
; Example Output: #t
(define (is-adjacent state1 state2)
    (contains state2 (nth-item (index-of state1 adjacency-map) adjacency-map))
)



; helper function for possible-swaps
; Algorithm :    The function will swap the first element with everybody else down the list 
;                It will then swap the next element with everybody else down the list. and so on until it
;                reaches the end of the list. Instead of swapping. its saving the indexes of what will be swapped
(define (possible-swaps-helper list-length swap1 swap2 swap-list)
    (cond
        (
            (equal? swap2 list-length)
                (possible-swaps-helper list-length (+ swap1 1)  (+ swap1 2) (cons (list swap1 swap2) swap-list ))
        )
        (
            (> swap2 list-length)
                swap-list
        )
        (
            #t
                (possible-swaps-helper list-length swap1  (+ swap2 1) (cons (list swap1 swap2) swap-list ))
        )
    )
)

; possible-swaps: returns a list of the possible combination of swaps using the indexes of the list
; Example Input: (possible-swaps 5 1 2 '())
; Example Output: ((4 5) (3 5) (3 4) (2 5) (2 4) (2 3) (1 5) (1 4) (1 3) (1 2))
(define (possible-swaps list-length)
    (possible-swaps-helper list-length 1 2 '())
)


; helper function to get-children
; Parameters:    state-list     = the list that you want to see the possible child states for (possible swaps)
;                swap-list      = this is the output of possible swaps, feeding this function what to swap
;                children-list  = where the output is being stored. 
;                swap-state     = keeps track of the swaps it took to get to the child state
;
; Algorithm:    For every element in the swap-list, perform that swap upon the state list, and append that swap-index-pair 
;               to the swap-state. 
(define (get-children-helper state-list swap-list children-list swap-state)
    (cond
        (
            (null? swap-list)
                children-list
        )
        (
            #t
                (get-children-helper state-list (cdr swap-list) (cons (cons (swap-element (nth-item 1 (car swap-list)) (nth-item 2 (car swap-list)) state-list)  (list(cons (car swap-list) swap-state))) children-list) swap-state)
        )
    )
)

; get-children: Used to determine all possible states 1 move away from the provided given state (state-list)
; Input Example: (get-children '(A B C) '() '((1 2)))
; Output Example: (((B A C) ((1 2) (1 2))) ((C B A) ((1 3) (1 2))) ((A C B) ((2 3) (1 2))))
(define (get-children state-list result swap-state)
    (get-children-helper state-list (possible-swaps (list-length state-list)) result swap-state)
)


; helper function to is-goal-state
; Algorithm: for every element in the statelist, check its neighbors to see if the are adjacent
(define (is-goal-state-helper state-list)
    (cond
        (
            (null? (cdr state-list))
                #t
        )
        (
            #t
                (if (is-adjacent (car state-list) (car (cdr state-list)))
                    (is-goal-state-helper (cdr state-list))
                    #f
                )
        )
    )
)

; is-goal-state: Returns a boolean of is the the provided states list is a goal state
; GOAL STATE DEFINITION: a list that can be trasversed on a map crossing border to border
; Example Input: (is-goal-state '((California Washington Oregon)))
; Example Output: #f
(define (is-goal-state child)
    (is-goal-state-helper (car child))
)

; ldfs for limited depth first search 
; helper function for ldfs. 
; Parameters:   frontier        = the queue of states to check
;               max-depth       = maximum depth (number of swaps) allowed to perform from given starting state
;               counter         = used to track the current depth 
;               state-history   = used to keep track of visited states to prevent infinite loops
; NOTE: 'MARKER is appended to the end of the "added children list" to indicate to pop back up 
;                         ABC
;                      /   |    \
;                  BAC    CBA    ACB  'MARKER   
;              /   |    \    
;            ABC  CBA   CBA 'MARKER
(define (ldfs-helper frontier max-depth counter state-history)
    (cond 
        (   ; FAILED: Exhausted the frontier. No more possible states to check. 
            (null? frontier) 
                #f 
        )
        (   ; Reached the End of an added children list. Pop back up 
            (equal? (car frontier) 'MARKER)
                (ldfs-helper  (cdr frontier) max-depth (- counter 1) state-history)
        )
        (   ; SUCCESS: Found the goal state! Return the result
            (is-goal-state (car frontier) ) ; Check the head of the frontier if it's the goal state
                (list (car (car frontier)) (reverse (car(cdr (car frontier)))))
        )
        (   ; Already saw a state like this.. No need to add its children back into the frontier
            (contains (car (car frontier)) state-history )
                (ldfs-helper  (cdr frontier) max-depth counter state-history )
        )
        (   ; Reached the Depth Limit. Check your neighbor child. (same as popping back to parent and checking 
            ; your sibling)
            (>= counter max-depth)
                (ldfs-helper  (cdr frontier) max-depth counter (cons (car (car frontier)) state-history) )
        )
        (   ; Default Case: Find the children of the state you are in and append them to the frontier
            #t 
                (if (= counter max-depth)
                    (ldfs-helper  (cdr frontier) max-depth counter (cons (car (car frontier)) state-history))
                    (ldfs-helper (append (append (get-children (car (car frontier)) '(MARKER) (car(cdr (car frontier)))) ) (cdr frontier)) max-depth (+ counter 1) (cons (car (car frontier)) state-history) )
                    
                )
        )
    )
)


; Example Input: (ldfs '(California Washington Oregon) 4)
; Example Output: ((California Oregon Washington) ((1 2) (1 3) (1 2)))

; Example Input: (ldfs '(California Washington Idaho Arizona Oregon Montana) 5)
; Example Output: ((Arizona California Oregon Washington Idaho Montana) ((1 2) (1 4) (1 5) (1 3) (1 5)))

; Example Input: (ldfs '(California Washington Idaho Arizona Oregon Montana Florida) 5)
; Example Output: #f

; Example Input: (id-dfs '(California))
; Example Output: ((California) ())
(define (ldfs state-list max-depth)
    (if (<= (list-length state-list) 1)
        (list state-list '())
        (ldfs-helper (get-children state-list '() '()) max-depth 1 (list state-list))
    )
)

; Rationale for list-length for max-depth: This is under the rationale that. in the worst case solution and given
; that there is a solution, you can ALWAYS solve the problem in n swaps (n beigng length). It is under the same rationale 
; as selection sort. It takes 1 swap to put a state into its correct state. With the max depth being length, I believe
; dfs should be able to encapsulate this scenario. Hopefully I'm right :)... Fingers crossed. This is my own thoughts.
; no math to prove it. sorry. 

; OTHERWISE....
; My Initial Thoughts: Initially I thought it may have needed to be the permutation for max depth since this 
; would cover every literal combination possible. AND it would still work! I just think the length... may be 
; a the best minimum value to cover everything... (hopefully I'm right)

; TO BE OPTIMAL (Minimal Amount of Swaps):
; I would start from list-length, if I find a result. Increment down until you get your first #f. Last success (minimal depth needed)
; will be the optimal in terms of swap.. but seems very costly. I guess thats why we need heuristics (to not exhaustively search)

; Example Input: (id-dfs '(California Washington Oregon))
; Example Output: ((California Oregon Washington) ((1 2) (1 3) (1 2)))

; Example Input: (id-dfs '(California Washington Idaho Arizona Oregon Montana))
; Example Output: ((Arizona California Oregon Washington Idaho Montana) ((1 2) (1 4) (1 5) (1 3) (1 5)))

; Example Input: (id-dfs '(California Washington Idaho Arizona Oregon Montana Florida))
; Example Output: #f
(define (id-dfs state-list)
    (ldfs state-list (list-length state-list))
)



