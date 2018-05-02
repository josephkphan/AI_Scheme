; Author:       Joseph Phan
; Class:        Coen 266 AI
; Assignment 3: Inferring 
; Date:         Spring 2018
; ------------------------------- Input Data -----------------------------------
; TEMPLATE
; Function Description: 
; Example Input: 
; Example Output:
;
; Note: Every function has a brief description of what the function does as well as 
; unit-tested statements / examples that can be executed in the guile interpreter 
;
; **LINGO **      
; Argument -------------------- represented by a letter in a list. It can be negated i.e.  a or (NOT a)     
; Statement ------------------- Represented by a list of arguments  (a (NOT b) c). statements in an abbreviated form
;                               of conjunctive normal form (CNF), where each argument is a list of elements that are implicitly
;                               or’d together (a disjunction of variables). 
; Pair of Negated Arguments --- When two statements have a common variable, 
;                               except one is negated i.e. '( (NOT a) b)  and ((a) b) where a is the negated pair



; Function Description: returns the next statement of the list WITHOUT negation
; Example Input: (next '((NOT f) b))
; Example Output: f
; Example Input: (next '(d (NOT f) b))
; Example Output: d
(define (next alist)
    (if (list? (car alist)) 
        (car (cdr (car alist)))        ; Is an NOT value. Return that value
        (car alist)    ;Not an inverse     
    )
)

; Function Description: returns a boolean value whether or not the next statement in the list is negated
; Example Input: (next '((NOT f) b))
; Example Output: #t
; Example Input: (next '(d (NOT f) b))
; Example Output: #f
(define (next-is-inverse alist)
    (list? (car alist))
)

; Function Description: returns a boolean value whether or not a negatived-given-argument is found in the statement
; Example Input: (contains-inverse 'f '(d (NOT f) b))
; Example Output: #t
; Example Input: (contains-inverse '(NOT f) '(d (NOT f) b))
; Example Output: #f
(define (contains-inverse item alist)
    (cond 
        (   ; Traversed through whole list - didn't find an inverse
            (null? alist)
                #f
        )
        (   ; Condition if the item in comparison is an inverse
            (list? item)
            (if (and (not (next-is-inverse alist))  (equal? (car(cdr item)) (next alist))) 
                #t                                  ; Current head of list is an inverse Return True;
                (contains-inverse item (cdr alist)) ; Head of list was not inverse, check next element
            )
        )
        (   ; item is not an inverse
            #t
            (if (and (next-is-inverse alist)  (equal? item (next alist))) 
                #t                                  ; Current head of list is an inverse Return True;
                (contains-inverse item (cdr alist)) ; Head of list was not inverse, check next element
            )
        )
    )
)

; Function Description: Helper function to iterate and count the number of negated pairs of arguments between two statements
(define (number-inverse-found-helper list1 list2 counter)
    (cond 
        (   ; Traversed through whole list - return the counter
            (null? list1)
                counter
        )
        (
            #t
            (if (contains-inverse (car list1) list2)
                ; Found an inverse 
                (number-inverse-found-helper (cdr list1) list2 (+ counter 1))
                ; Did not find an inverse
                (number-inverse-found-helper (cdr list1) list2 counter)
            )
        )
    )
)


; Function Description: returns a counter of the number of negated pairs of arguments found
; Example Input: (number-inverse-found '((NOT d) f b) '(d (NOT f) b))
; Example Output: #t
(define (number-inverse-found list1 list2)
    (number-inverse-found-helper list1 list2 0)
)

; Function Description: Helper function to concat Iterated through the corpus, adding any argument to the result if it cannot find its negation
(define (concat-helper corpus temp result)
    (cond
        (   ; Checked all values in corpus
            (null? temp)
                (reverse result)
        )
        (   ; Checking head to see if it has a inverse
            (if (contains-inverse (car temp) corpus)
                (concat-helper corpus (cdr temp) result)    ; move to next iteration
                (concat-helper corpus (cdr temp) (cons (car temp) result ))    ; append to result, move to next iteration
            )
        )
    )
)

; Function Description: returns a resolved statement from the two given statements (meaning it excludes all pairs of negated-arguments)
; Example Input: (concat '(a (NOT b) c) '(d (NOT f) b))
; Example Output: (a c d (NOT f))
(define (concat list1 list2)
    (concat-helper (append list1 list2) (append list1 list2) '())
)

;Function Description- This is a statement resolution function. It should accept two statements and return one of the following:
;   list ----------- resolved statement
;   #f   ----------- unresolved statement
;   CONTRADICTION -- contradiction found between statements
; 
; Example Input: (resolve '(a b) '((NOT b)))
; Example Output: (a)
;
; Example Input: (resolve '(a (NOT b) c) '(d (NOT f) b))
; Example Output: (a c d (NOT f))
;
; Example Input: (resolve '(a b) '(c d))
; Example Output:  #f
;
; Example Input: (resolve '(a b) '((NOT a) (NOT b)))
; Example Output: #f
;
; Example Input: (resolve '(a) '((NOT a)))
; Example Output: CONTRADICTION

(define (resolve list1 list2)
    (let ((inverse_count (number-inverse-found list1 list2)))
        (cond
            (
            (= inverse_count 0)
                #f ; not resolved
            )
            (
                (= inverse_count 1)
                    (if (and (= (length list1) 1) (= (length list2) 1) ) 
                        'CONTRADICTION  
                        (concat list1 list2)
                    )
            )
            (
                #t
                    (display "HERE")
                    #f
            )
        )
    )
)


