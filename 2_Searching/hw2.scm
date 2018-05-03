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
(define (id-dfs-helper3 frontier max-depth counter state-history)
    (cond 
        (   ; FAILED: Exhausted the frontier. No more possible states to check. 
            (null? frontier) 
                '() 
        )
        (   ; Reached the End of an added children list. Pop back up 
            (equal? (car frontier) 'MARKER)
                (id-dfs-helper3  (cdr frontier) max-depth (- counter 1) state-history)
        )
        (   ; SUCCESS: Found the goal state! Return the result
            (is-goal-state (car frontier) ) ; Check the head of the frontier if it's the goal state
                (list (car (car frontier)) (reverse (car(cdr (car frontier)))))
        )
        (   ; Already saw a state like this.. No need to add its children back into the frontier
            (contains (car (car frontier)) state-history )
                (id-dfs-helper3  (cdr frontier) max-depth counter state-history )
        )
        (   ; Reached the Depth Limit. Check your neighbor child. (same as popping back to parent and checking 
            ; your sibling)
            (>= counter max-depth)
                (id-dfs-helper3  (cdr frontier) max-depth counter (cons (car (car frontier)) state-history) )
        )
        (   ; Default Case: Find the children of the state you are in and append them to the frontier
            #t 
                (if (= counter max-depth)
                    (id-dfs-helper3  (cdr frontier) max-depth counter (cons (car (car frontier)) state-history))
                    (id-dfs-helper3 (append (append (get-children (car (car frontier)) '(MARKER) (car(cdr (car frontier)))) ) (cdr frontier)) max-depth (+ counter 1) (cons (car (car frontier)) state-history) )
                    
                )
        )
    )
)


; Example Input: (id-dfs-helper2 '(California Washington Oregon) 4)
; Example Output: ((California Oregon Washington) ((1 2) (1 3) (1 2)))

; Example Input: (id-dfs-helper2 '(California Washington Idaho Arizona Oregon Montana) 5)
; Example Output: ((Arizona California Oregon Washington Idaho Montana) ((1 2) (1 4) (1 5) (1 3) (1 5)))

; Example Input: (id-dfs-helper2 '(California Washington Idaho Arizona Oregon Montana Florida) 5)
; Example Output: #f

; This is used to handle cases when states length <=2
; Example Input: (id-dfs '(California))
; Example Output: ((California) ())
(define (id-dfs-helper2 state-list max-depth)
    (cond 
        (
            (<= (list-length state-list) 1)
                (list state-list '())
        )
        (
            (= (list-length state-list) 2)
                (if (is-adjacent (car state-list) (car (cdr state-list)))
                    (list state-list '())
                    '() ; 
                )
        )
        (
            #t
                (id-dfs-helper3 (get-children state-list '() '()) max-depth 1 (list state-list))
        )
    )
)

; Rationale for list-length for max-depth: This is under the rationale that. in the worst case solution and given
; that there is a solution, you can ALWAYS solve the problem in n swaps (n being length). It is under the same rationale 
; as selection sort. It takes 1 swap to put a state into its correct state. With the max depth being length, I believe
; dfs should be able to encapsulate this scenario. Hopefully I'm right :)... Fingers crossed. This is my own thoughts.
; no math to prove it. sorry. 

; OTHERWISE....
; My Initial Thoughts: Initially I thought it may have needed to be the permutation for max depth since this 
; would cover every literal combination possible. AND it would still work! I just think the length... may be 
; a the best minimum value to cover everything... (hopefully I'm right)

; EDIT: 
; Turns out you only need length -1 (as specified from email from professor)
; Doesn't hurt to leave it at n. theoretically it will still find a solution there. 

; TO BE OPTIMAL (Minimal Amount of Swaps):
; I would start from list-length, if I find a result. Increment down until you get your first #f. Last success (minimal depth needed)
; will be the optimal in terms of swap.. but seems very costly. I guess thats why we need heuristics (to not exhaustively search)


; This will start id-dfs to start with a max depth of 1 ... n where n is list of length
; It will return when it finds the first possible solution
; If it iteratives through 1 .. n without finding a success state. It will return false

(define (id-dfs-helper state-list counter max)
    (cond 
        (   ; Could not find solution
            (> counter max)
                #f
        )
        (   ; Found Solution
            (not (null?  (id-dfs-helper2 state-list counter)))
                (id-dfs-helper2 state-list counter)
        )
        (   ; Keep Iterating
            #t
                (id-dfs-helper state-list (+ counter 1) max)
        )    
    )
)

; Example Input: (id-dfs '(California Washington Oregon))
; Example Output: ((California Oregon Washington) ((1 2) (1 3) (1 2)))

; Example Input: (id-dfs '(California Washington Idaho Arizona Oregon Montana))
; Example Output: ((Arizona California Oregon Washington Idaho Montana) ((1 2) (1 4) (1 5) (1 3) (1 5)))

; Example Input: (id-dfs '(California Washington Idaho Arizona Oregon Montana Florida))
; Example Output: #f
(define (id-dfs state-list)
    (id-dfs-helper state-list 1 (list-length state-list))
)


; ---------------------------- A* Functions -------------------------------
; Definition of Heuristic - How close you are to a solution

; My Heuristic - How "Sorted" are you? (I know... very Naive and simple .. with faults.. which I'll get to)

; Given a list (A B C D), you earn 1 point for every neighbor you are actually adjacent to (via adjacency map)
; here if (A B C D) is the goal state, it gets 6 points (since everybody was adjacent to each other)
;       +  1 2 2 1    = 6 
; Lets say B was ONLY adjacent to C   (also meaning A is not adjacent to B )
;        (A B C D)
;       + 0 1 2 1     = 4

; Analyzing this heuristic I can already see its faults:
;     - Doesn't work well if it requires "shifting"  
;           i.e.    if (A B C D was the only goal state) (B C D A) will produce a high heuristic, but still cost a large
;                   number of swaps to get it to the goal state still
;     (meaning, if there are partial correctly "sorted" portions of the state. it will product a false-positive high heuristic)
  

; give-heuristic-point: returns 1 if adjacent, and 0 if it is not
; Example Input: (give-heuristic-point 'California 'Oregon)
; Exampme Output: 1

(define (give-heuristic-point state1 state2)
    (if (is-adjacent state1 state2)
        1  ; is adjacent
        0  ; not adjacent
    )
)
(define (generate-heuristic-value-helper state-list index result)
    (cond
        (   ; Finished iterating across all elements in list
            (> index (list-length state-list))
                result 
        )
        (   ; first index   - since it only has a right neighbor
            (= index 1)
                (generate-heuristic-value-helper state-list (+ index 1) 
                    (+ result (give-heuristic-point 
                        (nth-item index state-list) 
                        (nth-item (+ index 1) state-list)     
                        ) 
                    )
                )
        )
        (   ; last index    - since it only has a left neighbor
            (= index (list-length state-list))
                (generate-heuristic-value-helper state-list (+ index 1) 
                    (+ result (give-heuristic-point 
                        (nth-item index state-list) 
                        (nth-item (- index 1) state-list)     
                        ) 
                    )
                )
        )  
        (   #t
            ; anything in the middle
                (generate-heuristic-value-helper state-list (+ index 1) 
                    (+ 
                        (+ result (give-heuristic-point 
                            (nth-item index state-list) 
                            (nth-item (- index 1) state-list)     
                            ) 
                        )
                        (give-heuristic-point 
                            (nth-item index state-list) 
                            (nth-item (+ index 1) state-list)     
                        ) 
                        
                    )
                )
        )
    )
)

; generate-heuristic-value: Given a state's list, it will produce a heuristic value
; Example input:
; Example output:
(define (generate-heuristic-value state-list)
    (generate-heuristic-value-helper state-list 1 0)
)

; A*get-children-helper; just like get-children helper, BUT adds the heuristic value to the child (see output example)
; Example Input: (A*get-children '(California Oregon Washington) '() '((1 2)))
; Example Output: ((((Oregon California Washington) ((1 2) (1 2))) 2) (((Washington Oregon California) ((1 3) (1 2))) 4) (((California Washington Oregon) ((2 3) (1 2))) 2))
(define (A*get-children-helper state-list swap-list children-list swap-state)
    (cond
        (
            (null? swap-list)
                children-list
        )
        (
            #t
                (A*get-children-helper 
                    state-list 
                    (cdr swap-list) 
                    (cons (cons (cons (swap-element (nth-item 1 (car swap-list)) (nth-item 2 (car swap-list)) state-list)  
                        (list(cons (car swap-list) swap-state))) 
                            (list(generate-heuristic-value (swap-element (nth-item 1 (car swap-list)) (nth-item 2 (car swap-list)) state-list)))
                            )
                        children-list) 
     
                    swap-state
                )
        )
    )
)

(define (A*get-children state-list result swap-state)
    (A*get-children-helper state-list (possible-swaps (list-length state-list)) result swap-state)
)


; Assumption: list is unique! This only works to remove the first element
; Assumption: dealing with numbers
; Example Input: (remove-item 1 '(1 2 3))
; Example Output: (2 3)
(define (remove-item item alist)
    (cond 
        (   
            (null? alist) 
                '() 
        )           
        ( 
            (equal? item (car alist) ) 
                (cdr alist)
        )
        (   
            #t 
                (cons (car alist) (remove-item item (cdr alist) ))
        ) 
    )
)

; Selection sort to get the biggest child from the frontier
; Example Input: (selection-sort '((((Oregon California Washington) ((1 2)(1 2))) 2) (((Washington Oregon California) ((1 3) (1 2))) 4) (((California Washington Oregon) ((2 3) (1 2))) 2)))
; Example Output: ((((Oregon California Washington) ((1 2) (1 2))) 2) (((California WashingtonOregon) ((2 3) (1 2))) 2) (((Washington Oregon California) ((1 3) (1 2))) 4))
(define (selection-sort alist) 
    (cond 
        ( 
            (null? alist) 
                '() 
        )
        ( 
            #t 
                (cons (biggest alist) (selection-sort (remove-item (biggest alist) alist)))
        )
    )
)

; Finds the biggest heuristic value of frontier. Used for Selection Sort   
(define (biggest-helper min result alist)
    (cond 
        ( 
            (null? alist) 
                result
        )
        ( 
            (> (nth-item 2 (car alist)) min) 
                (biggest-helper (nth-item 2 (car alist)) (car alist) (cdr alist)))
        (
            #t 
                (biggest-helper min result (cdr alist)  )
        )
    )
)

; biggest - returns the item in the list with the biggest heuristic value
; Assumption: alist is not empty
; input: Takes in the frontier
(define (biggest alist) 
    (biggest-helper (nth-item 2 (car alist)) (car alist) alist)
)


; Algorithm for A* 
; Note that upon get children creation - when adding it to the frontier- it is then shorting the frontier from 
; largest to biggest. That way the head of the list is the child with the highest heuristic value
(define (A*-helper3 frontier max-depth state-history)
    (cond 
        (   ; FAILED: Exhausted the frontier. No more possible states to check. 
            (null? frontier) 
                '() 
        )
        (   ; SUCCESS: Found the goal state! Return the result
            (is-goal-state (car (car frontier) )) ; Check the head of the frontier if it's the goal state
                (list (car (car (car frontier))) (reverse (car(cdr (car (car frontier))))))
        )
        (   ; Already saw a state like this.. No need to add its children back into the frontier
            (contains (car (car frontier)) state-history )
                (A*-helper3  (cdr (car frontier)) max-depth  state-history )
        )
        (   ; Reached the Depth Limit. Check your neighbor child. (same as popping back to parent and checking 
            ; your sibling)
            (>= (list-length(car (cdr (car (car frontier))))) max-depth)
                (A*-helper3  (cdr frontier) max-depth state-history)
        )
        (   ; Default Case: Find the children of the state you are in and append them to the frontier
            #t 
                (if (= (list-length(car (cdr (car (car frontier))))) max-depth)
                    (A*-helper3  (cdr frontier) max-depth state-history)
                    (A*-helper3 (selection-sort (append (append (A*get-children (car (car (car frontier))) '() (car(cdr (car (car frontier)))))) (cdr frontier)))
                                 max-depth 
                                 state-history)
                                 ;(append (append (get-children (car (car frontier)) '(MARKER) (car(cdr (car frontier)))) ) (cdr frontier)) max-depth (+ counter 1) (cons (car (car frontier)) state-history) )

                )
        )
    )
)

; Used to check whether or not state list has length <= 2
(define (A*-helper2 state-list max-depth)
    (cond 
        (
            (<= (list-length state-list) 1)
                (list state-list '())
        )
        (
            (= (list-length state-list) 2)
                (if (is-adjacent (car state-list) (car (cdr state-list)))
                    (list state-list '())
                    '() ; 
                )
        )
        (
            #t
                (A*-helper3 (A*get-children state-list '() '()) max-depth (list state-list))
        )
    )
)

; Used to iterate through the different depths
(define (A*-helper state-list counter max)
    (cond 
        (   ; Could not find solution
            (> counter max)
                #f
        )
        (   ; Found Solution
            (not (null?  (A*-helper2 state-list counter)))
                (A*-helper2  state-list counter)
        )
        (   ; Keep Iterating
            #t
                (A*-helper state-list (+ counter 1) max)
        )    
    )
)

; Example Input: (A* '(California Washington Idaho Arizona Oregon Montana Nevada ))
; Example Output: ((Montana Idaho Washington Oregon California Arizona Nevada) ((1 5) (4 6) (1 4) (2 3)))
(define (A* state-list)
    (A*-helper state-list 1 (list-length state-list))
)
