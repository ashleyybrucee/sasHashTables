#lang rosette

; Declarations
(define itemsInArray 0)
(define worstCase 0)
(define counter 0)
(define arraySize 10)
(define sameVal 0)
(define v (make-vector arraySize))
(vector-fill! v -1)

; Inserts a key to a given index
(define (insert index key)
  (vector-set! v index key))

; Method that increases by one
(define (addOne num)
  (+ num 1))

; Simple hash function method
(define (hashFunction key) 
  (abs (remainder key arraySize))
  )


(define (hashFunction2 key)
(abs (remainder (+ (* key 3) (remainder 13)) arraySize))
  )

; Determines whether the worst case counter
; needs to be incremented 
(define (isWorstCase)
  (define newCounter (addOne counter))
  (cond
    [(= newCounter itemsInArray)
     (set! worstCase (addOne worstCase))]
  ))

; Hash function
(define (hash key)
  (collisionResolution (hashFunction key) key)
  )

; Resolves collisions within table
(define (collisionResolution index key)
  (define num (vector-ref v index))

  (cond
       [(= num key)
        (set! sameVal (addOne sameVal))
        (set! counter 0)]
       [(= -1 num)
         (insert index key)
         (set! itemsInArray (addOne itemsInArray))
         (isWorstCase)
         (set! counter 0)]
       [else
         (set! counter (addOne counter))
         
         (collisionResolution (remainder (addOne index) arraySize) key)
    
         ])
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Method for adding three numbers into
; the hash table in one call
(define (addNums x y z a b c d e)
 
  (hash x)
  (hash y)
  (hash z)
  (hash a)
  (hash b)
  (hash c)
  (hash d)
  (hash e)

  (display "worstCase: ")
  (display worstCase)
  (display " itemsInArray: ")
  (display itemsInArray)
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(current-bitwidth 8)
(define-symbolic* x y z a b c d e integer?)
(define hashSame (solve+))

; Synthesis of worst-case input
;(hashSame
 ;  (= (hashFunction x) (hashFunction y) (hashFunction z)))


; Another way to synthesize
(define sol (solve (assert (= (hashFunction x) (hashFunction y) (hashFunction z) (hashFunction a) (hashFunction b)
                              (hashFunction c) (hashFunction d) (hashFunction e) )) ))
;(evaluate x sol)
;(evaluate y sol)
;(evaluate z sol)
;(evaluate a sol)
;(evaluate b sol)
;(evaluate c sol)
;(evaluate d sol)
;(evaluate e sol)

; How to make sure there are no duplicates
; Assertions necessary for 8 symbolic variables
(assert (= (hashFunction x) (hashFunction y)
           (hashFunction z) (hashFunction a)
           (hashFunction b) (hashFunction c)
          (hashFunction d) (hashFunction e)
          ))
(assert (not (= x y)))
(assert (not (= x z)))
(assert (not (= x a)))
(assert (not (= x b)))
(assert (not (= x c)))
(assert (not (= x d)))
(assert (not (= x e)))
(assert (not (= y z)))
(assert (not (= y a)))
(assert (not (= y b)))
(assert (not (= y c)))
(assert (not (= y d)))
(assert (not (= y e)))
(assert (not (= z a)))
(assert (not (= z b)))
(assert (not (= z c)))
(assert (not (= z d)))
(assert (not (= z e)))
(assert (not (= a b)))
(assert (not (= a c)))
(assert (not (= a d)))
(assert (not (= a e)))
(assert (not (= b c)))
(assert (not (= b d)))
(assert (not (= b e)))
(assert (not (= c d)))
(assert (not (= c e)))
(assert (not (= d e)))

