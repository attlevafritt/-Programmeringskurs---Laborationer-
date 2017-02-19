#lang racket
;;Laboration 2
(provide (all-defined-out))
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
  (lambda (the-list)
    (count-list-iter the-list 0)))

;;en iterativ lösning som ser till att beräkna
;;antalet element i varje lista som matas in. 
(define count-list-iter
  (lambda (the-list result)
    (if (pair? the-list)
        (count-list-iter (cdr the-list) (+ result 1))
        result)))

;;Del B

(define count-list-correct?
  (lambda (the-list)
    (= (length the-list) (count-list the-list))))

;;UPPGIFT 4 
(define keep-if
  (lambda (pred input)
    (if (null? input)
        '()
        (if (pred (car input))
            (cons (car input) (keep-if pred (cdr input)))
            (keep-if pred (cdr input))))))


(define keep-if-correct?
  (lambda (pred the-list)
    (equal? (keep-if pred the-list) (filter pred the-list))))


;;UPPGIFT 5
;;Del A
(define first-n
  (lambda (n list)
    (if (< (count-list list) n)
        list
        (if (= n 0)
           '()
           (cons (car list) (first-n (- n 1) (cdr list)))))))


;;Del B
(define first-n-correct?
  (lambda (n input)
    (if (> n (length input))
        #t
        (equal? (take input n) (first-n n input))
            )))


(first-n-correct? 6 '(3 4 7 2 4 8)) ;(1)
(first-n-correct? 3 '(34 2 23 3))

(first-n 8 '(2 3)) ;(3)
;; "first-n" returnerar hela listan för n som är längre än listan. "take" kan inte ta ett sådant argument.
;;se kör exempel (3)

