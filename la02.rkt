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
    (if (= (length the-list) (count-list the-list))
        #t
        #f)))

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
        (if (equal? (take input n) (first-n n input))
            #t
            #f))))


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

;;iterativ rekursiv
(define reverse-order-iter
  (lambda (list)
    (reverse-order-iter2 list (count-list list))))

(define reverse-order-iter2
  (lambda (list count)
    (if (= count 0)
        '()
        (mend-lists (reverse-order-iter2 (cdr list) (- count 1)) (cons (car list) '())))))


;;UPPGIFT 8 
(define map-to-each
  (lambda (pred list)
    (if (null? list)
        '()
        (cons (pred (car list)) (map-to-each pred (cdr list)))))) 


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

;;Huvud proceduren, en iterativ lösning. 
(define insert-sort
  (lambda (the-list)
    (insert-sort-iter the-list '())))


(define insert-sort-iter
  (lambda (the-list result)
    (if (null? the-list)
        result
    (insert-sort-iter (cdr the-list)
                  (insert-at-asc-place (car the-list) result)
                  ))))


    
    
;;UPPGIFT 10

;;beräknar alla element t.o.m. underlistor
(define count-all
  (lambda (the-list)
    (cond ([null? the-list] 0)
          ([pair? the-list] (+  (count-all (car the-list)) (count-all (cdr the-list))))
          (else 1))))





;;UPPGIFT 11 
(define occurs?
  (lambda (arg the-list)
    (cond ([eqv? arg the-list] #t)
          ([pair? the-list] (if (or (occurs? arg (car the-list)) (occurs? arg (cdr the-list)))
                                #t
                                #f))
          (else #f))))


;;UPPGIFT 12 
(define subst-all
  (lambda (s-out s-in the-list)
    (if (null? the-list) 
        '()
        (if (not (atom? (car the-list)));;om det finns underlistor 

            (cons (subst-all s-out s-in (car the-list)) (subst-all s-out s-in (cdr the-list)))
            (if (eq? (car the-list) s-out)
                (cons s-in (subst-all s-out s-in (cdr the-list)))
                (cons (car the-list) (subst-all s-out s-in (cdr the-list))))))))

;;UPPGIFT 13
(define keep-if-all
  (lambda (pred list)
    (cond ([null? list] '())
          ([atom? list] (if (pred list)
                            list
                            '()))
          ([pair? (car list)] (cons (keep-if-all pred (car list))
                                    (keep-if-all pred (cdr list))))
          (else (if (pred (car list))
                    (cons (car list) (keep-if-all pred (cdr list)))
                    (keep-if-all pred (cdr list)))))))



;;UPPGIFT 14 
(define list-equal?
  (lambda (list1 list2)
    (cond [(and (null? list1) (null? list2)) #t]
          [(or (null? list1) (null? list1)) #f]
          [(and (atom? list1) (atom? list2)) (eqv? list1 list2)]
          [(or (atom? list1) (atom? list2)) #f]
          (else (and (list-equal? (car list1) (car list2))
                     (list-equal? (cdr list1) (cdr list2)))))))





  
  








