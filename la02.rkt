(define even?
  (lambda (n)
    (if (= (remainder n 2) 0)
        #t
        #f)))

;UPPG 2
(define atom?
  (lambda (n)
    (cond ([list? n] #f)
          ([pair? n] #f)
          (else #t))))

;UPPG 3
;A
(define count-list
  (lambda (n)
    (count-list-iter n 0)))

(define count-list-iter
  (lambda (n result)
    (if (pair? n)
        (count-list-iter (cdr n) (+ result 1))
        result)))

;B
(define count-list-correct?
  (lambda (n)
    (if (= (length n) (count-list n))
        #t
        #f)))

;UPPG 4 oklar
(define keep-if
  (lambda (pred input)
    (if (symbol? (cdr input))
        input
        (if (pred (car input))
            (keep-if pred (cdr input))
            (keep-if pred (cdr (cdr input)))))))

(keep-if even? '(1 2 3 4))
