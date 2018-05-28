(let ((time (gettimeofday)))
      (set! *random-state*
            (seed->random-state (+ (car time)
                                   (cdr time))))
)

; Example-Percept: 
;        -5 -4 -3 -2 -1 0  1  2  3  4  5
;      5  B  B E  V4 E  E  E  V1 B  B  B
;      4     B E  E  E  E  E  E  B  B
;      3       V3 E  E  E  E  E  B
;      2          E  V2 E  E  E
;      1             E  E  E
;      0                A

; -------------  Useful Functions -------------
(define example-percept '((empty empty empty)
(empty (vegetation 2 45) empty empty empty)
((vegetation 3 150) empty empty empty empty empty barrier)
(barrier empty empty empty empty empty empty barrier barrier)
(barrier barrier empty (vegetation 4 200) empty empty empty
(vegetation 1 125) barrier barrier barrier))
)

; nth-item - get's the nth item from a list
; ASSUMPTION: List is not empty
(define (nth-item n alist)
    (if (= n 1) (car alist) (nth-item (- n 1) (cdr alist))) 
)


(define (get-x-row percept yCoord)
    (nth-item yCoord percept)
)

(define (translate-xCoord-to-index xCoord yCoord)
    (+ (+ xCoord yCoord) 1)
)

; Example Input/Output: 
;guile> (get-location percept 0 1)
;empty
;guile> (get-location percept -1 2)
;(vegetation 2 45)
;guile> (get-location percept 3 5)
;barrier
(define (get-location percept xCoord yCoord)
    (nth-item (translate-xCoord-to-index xCoord yCoord) (get-x-row percept yCoord))
)



; -------------  Main Logic Functions -------------
; Overall Prities:
; * Don't move too far, go for low hanging fruit always (nearby vegetation)
; 
; * If you got attacked. Run!  
; 
; * If you moved, and you don't a vegetation in front of you. Turn. This means you are either. 
;       1. running away from a predator
;       2. went to go looking for food, but it is now our of your field of vision
; 
; * Be aggressive for nearby agents when moving to a guarenteed food spot. Meaning, if food is
;   directly in front of you (straight line path), Rush to get there as it's the highest percentage to gain vegetation
;
; * If current energy falls below a certain threshold. just sit... and stall to gain more "turns" before death

(define (initialize-agent)
        "OK"
)




; ----------------------------------------------------------

(define (is-vegetation percepts coordinate) 
      (let ((coordinate-value (get-location percepts (car coordinate) (car (cdr coordinate))) ))
            (and 
                  (list? coordinate-value)   
                  (equal? (car coordinate-value) 'vegetation)
            )
      )
)

(define (is-valuable-vegetation percepts coordinate) 
      (let ((coordinate-value (get-location percepts (car coordinate) (car (cdr coordinate))) ))
            (and 
                  (and 
                        (list? coordinate-value)   
                        (equal? (car coordinate-value) 'vegetation)
                  )
                  (> (nth-item 3 coordinate-value) 50)
            )
      )
)

(define (is-barrier percepts coordinate)
      (let ((coordinate-value (get-location percepts (car coordinate) (car (cdr coordinate))) ))
            (and 
                  (not (list? coordinate-value))
                  (equal? coordinate-value 'barrier)
            )
      )
)

(define (is-empty percepts coordinate)
      (let ((coordinate-value (get-location percepts (car coordinate) (car (cdr coordinate))) ))
            (and 
                  (not (list? coordinate-value))
                  (equal? coordinate-value 'empty)
            )
      )
)

(define (is-predator percepts coordinate)
      (let ((coordinate-value (get-location percepts (car coordinate) (car (cdr coordinate))) ))
            (and 
                  (list? coordinate-value)   
                  (equal? (car coordinate-value) 'predator)
            )
      )
)

; ----------------------------------------------------------

(define (is-valuable-vegetation-in-front percepts)
      (is-valuable-vegetation percepts '(0 1)) 
)

; The function is called when choosing Move forward. It will determine how aggresive to be.
; Possive reasons to move forward: found vegetation, avoid predators
; 
; Possible return values
;  -   -   B2  -  - 
;      B1  B4  B3
;          A
; None Found = (MOVE-PASSIVE-1) 
; Barriers found at position B1 = Return (TURN-RIGHT)
; Barriers found at position B2 = Return (TURN-AROUND)
; Barriers found at position B3 = Return (TURN-LEFT)
; Barriers found at position B4 = Return 
(define (is-barrier-next-to-vegetation percepts)
      (cond 
            (     ;barrier on west of vegetation. eat aggresively
                  (is-barrier percepts '(-1 1))  
                        #t
            )    
            (     ;barrier north of vegetation. eat aggresively
                  (is-barrier percepts '(0 2))  
                        #t
            )    
            (     ;barrier on east of vegetation. eat aggresively
                  (is-barrier percepts '(1 1))  
                        #t
            )    
            (     ; No barriers next to vegetation, safe to eat passively
                  #t
                        #f    
            )    
      )
)


; ----------------------------------------------------------

; (define (from-history previous-events)
;       (display "from-history:")(newline)
; )


(define (is-last-event-attacked previous-events)
      (cond
            (
                  (null? previous-events) 
                  #f
            )
            (
                  (equal? (car (car previous-events)) 'attacked-by)
                  #t
            )
            (
                  #t
                  (is-last-event-attacked (cdr previous-events))
            ) 
      )
)

(define (is-last-event-move previous-events)
      (cond
            (
                  (null? previous-events) 
                  #f
            )
            (
                  (equal? (car (car previous-events)) 'moved)
                  #t
            )
            (
                  #t
                  (is-last-event-move (cdr previous-events))
            ) 
      )
)




; ----------------------------------------------------------
; Will check the next x levels for nearby food. 
; Note that X is determined by energy level. 
; Possible Returns values:
; () no vegetation found.

 
(define vegetation-percept-coordinates-list '(
      (-1 1) (1 1) 
      (-1 2) (0 2) (1 2)
      (-1 3) (0 3) (1 3)
      (-1 4) (0 4) (1 4)
      )
) 

(define (determine-move-to-vegetation coordinate)
      (cond 
            (
                  (= (car coordinate) 0)
                  (string-append "MOVE-PASSIVE-" (number->string (- (car (cdr coordinate)))))
                  ;(string-append "MOVE-AGGRESSIVE-" (number->string (- (car (cdr coordinate)))))

            )
            (
                  (= (car (cdr coordinate)) 1)
                  "MOVE-PASSIVE-1"
            )
            (
                  (= (car (cdr coordinate)) 2)
                  "MOVE-PASSIVE-2"
            )
            (
                  (= (car (cdr coordinate)) 3)
                  "MOVE-PASSIVE-3"
            )
            (
                  (= (car (cdr coordinate)) 4)
                  "MOVE-PASSIVE-2"
                  ; Can't move directly to these spots anyways. move halfway there for now 
            )
      
      )

)

;(nearby-vegetation-helper 100 example-percept percept-coordinates-list)
(define (get-nearby-vegetation-coordinate percepts coordinate-list )
  (cond 
      ( 
            (null? coordinate-list)
                  '()
      )
      (
            (is-valuable-vegetation percepts (car coordinate-list))
                  ;(display (car coordinate-list))(newline)
                  ;(get-location percepts (car (car coordinate-list)) (car (cdr (car coordinate-list))))
                  (car coordinate-list)
      )
      (     
            #t
                  (get-nearby-vegetation-coordinate percepts (cdr coordinate-list))
      )
  )
)

(define (is-valuable-vegetation-nearby percepts coordinate-list)
      (cond 
            ( 
                  (null? coordinate-list)
                        #f
            )
            (
                  (is-valuable-vegetation percepts (car coordinate-list))
                        #t
            )
            (     
                  #t
                        (is-valuable-vegetation-nearby percepts (cdr coordinate-list))
            )
      )
)




; ----------------------------------------------------------

; Run AWay

(define (determine-run-away-strategy percepts)
      (if   
            (or (is-predator percepts '(0 1)) (is-vegetation percepts '(0 1)  ))
            "TURN-RIGHT"
            "MOVE-PASSIVE-3"
      )
)






; Precondition: agent is in a position to eat.
; depending if other agents are in position to eat will determine if agent should eat aggresively
; Possible return values
; () no predators nearby
;  -  -   B   -  - 
;     B   V   B
;         A
; If any Barriers are found at position B, eat aggresively (given the available energy)

(define (choose-action current-energy previous-events percepts)
(display "-------------------------------------------")(newline)
(display "current-energy:           ")(display current-energy)(newline)
(display "previous-events:          ")(display previous-events)(newline)
(display "Percepts:                 ")(display percepts)(newline)
(display "Making Move:              ")(display "MOVE")(newline)
      (cond 
            (
                  ;Out of Energy... Game Over. Sit til you die.
                  (< current-energy 20)
                  (display "!!!LOW-ENERGY \n")
                  "MOVE-PASSIVE-1"
            )
            (     ; Vegetation is in front of you. Eat!!
                  (is-valuable-vegetation-in-front percepts)
                  (display "!!!EAT \n")
                  (if (is-barrier-next-to-vegetation percepts)
                        "EAT-AGGRESSIVE"
                        "EAT-PASSIVE"
                  )
            )
            (     ; You moved last turn..This means you are searching for food. Look around! 
                  (is-last-event-move previous-events)
                  (display "!!!TURN \n")
                  (let ((rand (random 2)))
                        (cond ((equal? rand 0) "TURN-RIGHT")
                              ((equal? rand 1) "TURN-LEFT")
                              (#f "STAY")
                        )
                  )
            )
            (
                  (is-valuable-vegetation-nearby percepts vegetation-percept-coordinates-list)
                  (display "!!!FOOD \n")
                  (determine-move-to-vegetation (get-nearby-vegetation-coordinate percepts vegetation-percept-coordinates-list))
                  ; Go move toward food 
            )
            (
                  (is-last-event-attacked previous-events)
                  (display "!!!ATTACKED \n")
                  (determine-run-away-strategy percepts)
                  
                  ; Got attacked, and no new food to go to? still gotta GTFO for safety
                  ; This prays that the 
            )
            (
                  #t ; Look around for better things
                  (display "!!!RANDOM \n")
                  (let ((rand (random 5)))
                        (cond ((equal? rand 0) "TURN-RIGHT")
                              ((equal? rand 1) "TURN-LEFT")
                              ((equal? rand 2) "TURN-AROUND")
                              ((equal? rand 3) "MOVE-PASSIVE-1")
                              ((equal? rand 4) "MOVE-PASSIVE-2")
                              (#f "STAY")
                        )
                  )
                  
            )
            
      )


)