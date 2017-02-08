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

(trace reverser)
(reverser '(1 2 3 4 5 6 7 8))











