; Creator: Joseph Phan
; Homework 1: Scheming
; Coen266 
; Spring 2018

; ------ Basic Assignment Test Cases: 

(find-biggest '(1 2 3 4))
; 4

(find-biggest '(4 3 2 1))
; 4

(find-biggest '(1 9 14 9 12 11))
; 14



(count-from 1 2)
; 1
; 2

(count-from 14 21)
; 14
; 15
; 16
; 17
; 18
; 19
; 20
; 21

(count-from 101010 101020)
; 101010
; 101011
; 101012
; 101013
; 101014
; 101015
; 101016
; 101017
; 101018
; 101019
; 101020



(nth-item 4 '(1 2 3 4))
; 4

(nth-item 1 '((1 2) (3 4)))
; (1 2)

(nth-item 2 '(a b c d e f))
; b



(replace-nth-item 4 '(1 2 3 4) 14)
; (1 2 3 14)

(replace-nth-item 2 '(a b c d e) '(1 2 3))
; (a (1 2 3) c d e)

(replace-nth-item 1 '(((1 2 3) (4 5 6)) b c d) 'a)
; (a b c d)



(sorted? '(1 2 3 4))
; #t

(sorted? '(1 64 128))
; #t

(sorted? '(1 15 12 19 3))
; #f


(apply-action '(5 2 N) "MOVE-2")
; (5 4 N)

(apply-action '(0 1 W) "MOVE-1")
; (-1 1 W)

(apply-action '(14 -4 E) "STAY")
; (14 -4 E)

(apply-action '(0 0 S) "TURN-RIGHT")
; (0 0 W)

(apply-action '(1 2 E) "TURN-AROUND")
; (1 2 W)


; ------ Advanced Assignment Test Cases: 
(get-location
    '((empty empty empty)
    (empty (vegetation 2 45) empty empty empty)
    ((vegetation 3 150) empty empty empty empty empty barrier)
    (barrier empty empty empty empty empty empty barrier barrier)
    (barrier barrier empty (vegetation 4 200) empty empty empty
    (vegetation 1 125) barrier barrier barrier))
    0 1)
; empty

(get-location
    '((empty empty empty)
    (empty (vegetation 2 45) empty empty empty)
    ((vegetation 3 150) empty empty empty empty empty barrier)
    (barrier empty empty empty empty empty empty barrier barrier)
    (barrier barrier empty (vegetation 4 200) empty empty empty
    (vegetation 1 125) barrier barrier barrier))
    -1 2)
; (vegetation 2 45)

(get-location
    '((empty empty empty)
    (empty (vegetation 2 45) empty empty empty)
    ((vegetation 3 150) empty empty empty empty empty barrier)
    (barrier empty empty empty empty empty empty barrier barrier)
    (barrier barrier empty (vegetation 4 200) empty empty empty
    (vegetation 1 125) barrier barrier barrier))
    3 5)
; barrier




; ---- w/o solutions
(find-biggest '(1 2 3 4))

(find-biggest '(4 3 2 1))

(find-biggest '(1 9 14 9 12 11))

(count-from 1 2)

(count-from 14 21)

(count-from 101010 101020)

(nth-item 4 '(1 2 3 4))

(nth-item 1 '((1 2) (3 4)))

(nth-item 2 '(a b c d e f))

(replace-nth-item 4 '(1 2 3 4) 14)

(replace-nth-item 2 '(a b c d e) '(1 2 3))

(replace-nth-item 1 '(((1 2 3) (4 5 6)) b c d) 'a)

(sorted? '(1 2 3 4))

(sorted? '(1 64 128))

(sorted? '(1 15 12 19 3))

(apply-action '(5 2 N) "MOVE-2")

(apply-action '(0 1 W) "MOVE-1")

(apply-action '(14 -4 E) "STAY")

(apply-action '(0 0 S) "TURN-RIGHT")

(apply-action '(1 2 E) "TURN-AROUND")

(get-location
    '((empty empty empty)
    (empty (vegetation 2 45) empty empty empty)
    ((vegetation 3 150) empty empty empty empty empty barrier)
    (barrier empty empty empty empty empty empty barrier barrier)
    (barrier barrier empty (vegetation 4 200) empty empty empty
    (vegetation 1 125) barrier barrier barrier))
    0 1)

(get-location
    '((empty empty empty)
    (empty (vegetation 2 45) empty empty empty)
    ((vegetation 3 150) empty empty empty empty empty barrier)
    (barrier empty empty empty empty empty empty barrier barrier)
    (barrier barrier empty (vegetation 4 200) empty empty empty
    (vegetation 1 125) barrier barrier barrier))
    -1 2)

(get-location
    '((empty empty empty)
    (empty (vegetation 2 45) empty empty empty)
    ((vegetation 3 150) empty empty empty empty empty barrier)
    (barrier empty empty empty empty empty empty barrier barrier)
    (barrier barrier empty (vegetation 4 200) empty empty empty
    (vegetation 1 125) barrier barrier barrier))
    3 5)

