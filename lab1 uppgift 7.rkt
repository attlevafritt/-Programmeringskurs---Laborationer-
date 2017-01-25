(define last-digit
  (lambda (n)
    (remainder n 10)))

(define but-last-digit
  (lambda (n)
    (quotient n 10)))

(define sum-of-digits
  (lambda (n)
    (sum-of-digits-iter n 0)))
(define sum-of-digits-iter
  (lambda (n result)
    (if (= (but-last-digit n) 0)
        (+ result (last-digit n))
        (sum-of-digits-iter (but-last-digit n) (+ (last-digit n) result)))))

(define random-from-to
  (lambda (n m)
    (+ n (random m))))

(define simple-sv-sum?
  (lambda (n m)
    (if (= (remainder n m) 0)
        #t
        #f)))

(define make-simple-sv-sum
  (lambda (n)
    (let ([num (random-from-to 100000 900000)])
    (if (= (remainder (sum-of-digits num) n) 0)
        num
        (make-simple-sv-sum n)))))

(make-simple-sv-sum 2)
(simple-sv-sum? (make-simple-sv-sum 2) 2)
