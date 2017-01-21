#lang racket
;;Lab 1
;UPPGIFT 1

;(define foo (* 1 2 3 4))
;(define foobar (lambda () (* 1 2 3 4)))

#|SVAR: foo är ett definierat värde på 24, den returnerar
därmed 24. Det är ingen procedur och kan därmed inte 
evalueras som ett (d.v.s. "(foo)" ger error). 

foobar är en definierad procedure, 
som tar in ett tomt argument, men har i sig inget
värde i sig, därmed kan inte uttrycket "foobar" utan 
parantes evalueras. När "foobar" skrivs in i interaktionsfönstret
returneras därmed endast #<procedure: foobar>.|#

;;se nedan. 

#|Substitutionsmodellen: 
> foo
< 24
> foobar 
< #<procedure:foobar>
> (foo) 
>> application: not a procedure;
 expected a procedure that can be applied to arguments
  given: 24
  arguments...: [none]
>(foobar)
< 24
|#


;;UPPGIFT 2  
(require racket/trace)

;; Linjärrekursiva lösningen 
(define sum-rec 
  (lambda (n)
    #|Proceduren räknar nedifrån och adderar värdet på
    n till summan|#
    (cond [(= n 0) 0]
          [(= n 1) 1]
          [else (+ n (sum-rec (- n 1)))])))
        
#|Iterativt rekursiva lösnigen,
 om n=0 vill vi returnera resultatet.|#   
(define sum 
  (lambda (n)
    (sum-iter n 0)))

(define sum-iter
  (lambda (n result)
    #|Räkningen följer substitionsmetoden och 
      uppdaterar resultatet tills n=0|#
    (if (= n 0) 
        result
        (sum-iter (- n 1)
                  (+ n result)))))

(sum 5)

(trace sum-iter)
(trace sum-rec)

(printf "uppgift 3")
(newline)

;;UPPGIFT 3

#|Anledningen till att det inte fungerar är att labass
har använt "expt" som ett argument. Det han egentligen 
vill ådstakomma är en liknande funktion som den definierade
nedan (se vår lösning)|#


(define powers-of-two
  (lambda (n)
    (expt 2 n)))

(powers-of-two 16)

;;UPPGIFT 4
#|Låt dela in i tre tänkbara fall. 
När k=0 ges alltid 1 ut, när k=r ges alltid 1 ut. 
Annars beräknas talet av
(raden innan, kolumn innan)+(raden innan, samma kolumn)
|#


(define pascal
  (lambda (r k)
  (if (or (= k 0) (= k r))
      1
      (+ (pascal (- r 1) (- k 1)) 
            (pascal (- r 1) k)))))

(pascal 15 2)
(pascal 15 3)

;;UPPGIFT 5 
#|
Substitutionsmodellen för (pascal 4 3): 
>(pascal 4 3)
> (+ (pascal (- 4 1) (- 3 1)) (pascal (- 4 1) 3)))))
>>(+ (pascal (3 2)) (pascal (3 3)))
>> (+ 3 1)
<< 4 |#



