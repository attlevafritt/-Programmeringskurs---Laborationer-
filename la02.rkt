#lang racket
;;Laboration 2

(require racket/trace)

;;UPPGIFT 1
(define foo (cons 2 3))
(define bar (list 2 3))
(define forex (cons 2 (cons 3 4)))
(define tesco (list 2 bar))

;;UPPGIFT 2

(define atom?
  (lambda (n)
    (cond ([list? n] #f)
          ([pair? n] #f)
          (else #t))))

;;UPPGIFT 3
;;Del A

;;Huvudproceduren 
(define count-list
  (lambda (n)
    (count-list-iter n 0)))

;;en iterativ lösning som ser till att beräkna
;;antalet element i varje lista som matas in. 
(define count-list-iter
  (lambda (n result)
    (if (pair? n)
        (count-list-iter (cdr n) (+ result 1))
        result)))

;;Del B

(define count-list-correct?
  (lambda (n)
    (if (= (length n) (count-list n))
        #t
        #f)))

;;UPPGIFT 4 

(define even?
  (lambda (n)
    (if (= (remainder n 2) 0)
        #t
        #f)))

(define keep-if
  (lambda (pred input)
    (if (null? input)
        '()
        (if (pred (car input))
            (cons (car input) (keep-if pred (cdr input)))
            (keep-if pred (cdr input))))))

(keep-if even? '(1 2 3 4 5 6))

;;UPPGIFT 5
;;Del A
(define first-n
  (lambda (n list)
    (if (< (count-list list) n)
        list
        (if (= n 0)
           '()
           (cons (car list) (first-n (- n 1) (cdr list)))))))

(first-n 6 '(3 4 7 2 4 7 8))


;;Del B

(define first-n-correct?
  (lambda (n input)
    (if (equal? (take input n) (first-n n input))
        #t
        #f)))


(first-n-correct? 6 '(3 4 7 2 4 8)) ;(1)
(first-n-correct? 3 '(34 2 23 3))
(first-n 8 '(2 3)) ;(3)
;; "first-n" returnerar hela listan för n som är längre än listan. "take" kan inte ta ett sådant argument.
;;se kör exempel (3)


;;UPPGIFT 6

(define enumerate 
  (lambda (from to step)
    (if (> from to)
        '()
        (cons from (enumerate (+ step from) to step)))))

(enumerate 1 20 5)

;;UPPGIFT 7 
;;Del A 

;;"Klistrar ihop listor"
(define mend-lists
  (lambda (list1 list2)
    (if (null? list1)
        list2
        (cons (car list1) (mend-lists (cdr list1) list2)))))


;;linjär rekursiv
(define reverse-order-rek
  (lambda (list)
    (if (null? list)
        '()
        ;;ta ut varje element och göra en ny lista, sedan menda ihop skiten.
       (mend-lists (reverse-order-rek (cdr list)) (cons (car list) '()) ))))


;;UPPGIFT 8 
(define map-to-each
  (lambda (pred list)
    (if (null? list)
        '()
        (cons (pred (car list)) (map-to-each pred (cdr list)))))) 

(map-to-each sqrt '(1 2 3 4 5))
(map sqrt '(1 2 3 4 5))

;testfall gjort med map ok

;UPPGIFT 9

;;insättning till rätt plats, en redan sorterad lista.  
(define insert-at-asc-place 
  (lambda (number the-list)
    (if (null? the-list)
        (mend-lists (list number) the-list)
        (if (> (car the-list) number)
            (mend-lists (list number) the-list)
            (if (= (count-list the-list) 1) ;för number som placeras på slutet.
                (mend-lists the-list (list number))
                (mend-lists (list (car the-list)) 
                            (insert-at-asc-place number (cdr the-list)) ;sparar de element som ignoreras. 
                            ))))))

;;Huvud proceduren 
(define insert-sort-iter
  (lambda (the-list result)
    (if (null? the-list)
        result
    (insert-sort-iter (cdr the-list)
                  (insert-at-asc-place (car the-list) result)
                  ))))

(define insert-sort
  (lambda (the-list)
    (insert-sort-iter the-list '())))


(insert-sort '(1 8 3 4 5 8 10))
    
    
;;UPPGIFT 10


(define count-all
  (lambda (the-list)
    (cond [(null? the-list) '()]
          [(atom? the-list) 1]
          [(not (list? the-list)) 2]
          [else (count-all-iter the-list 0) ])))

(define count-all-iter
  (lambda (the-list counting)
    (if (null? the-list)
        counting
        (cond [ (pair? (car the-list)) (count-all-iter (cdr the-list) (count-list (car the-list)))]
              [(atom? (car the-list)) (count-all-iter (cdr the-list) (+ 1 counting))]))))

(trace count-all-iter)
(count-all '(1 2 3))              
(count-all '(1 (two 3 4) 5)) ;returnera 5

(count-all 1)
(count-all (cons 1 2))
    
    






