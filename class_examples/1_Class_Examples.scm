; Conditional Example
(display "Hello World!\n")

(cond ( (condition1 value)
        (condition2 value2)))

(cond ((( < num 0)          "Small")
        ((= 1 (% num 2))    "Odd")
        (#t                 "Dunno")
))

; Count the number of items in a list
; Think things in recursion in Scheme
(define (count-items alist)
        ; Base case = null list
        (cond ((null? alist ) 0)
        ; Take the rest of the list
                (#t (+ 1 (count-items (cdr alist)))))
        ; #t is used as an "else" to the first condition - default case
)

; (add-5 '(1 2 3)) should return (6 7 8)
(define (add-5 alist)
    ; Base case = empty list
        (cond   (null? alist) '())
                (#t (cons (+ 5 (car alist)))
                    (add-5 (cdr alist)))
)

; print downwards. if given 3 print 
; 3
; 2
; 1
(define (print-range val)
        (cond   (= val 0)) (newline)
                (#t (begin (display val) 
                    (display " ")
                    (print-range (- val 1)))
)

; print downwards. if given 3 print 
; 3
; 2
; 1
(define (print-range val)
        (cond   ((= val 0)) (display "0"))
                (#t (print-range-helper (- val 1))) 
                    (display val)
                    (display " ")
                    
)



; sum of every 2 items of a list
; input ( 1 7 14 -2 3)
; output (8 12 3)
(define (combine-pairs alist)
        (cond 
                ((null? alist) alist)
                ((null? (cdr alist)) alist)
                (#t (cons(+ (car alist) (car (cdr alist))) (combine-pairs (cdr(cdr alist)))))
                
        )
)




