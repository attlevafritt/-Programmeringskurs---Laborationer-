
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

;;UPPGIFT 6  

;;Hjälpproceduren "sum-of-digits"
(define sum-of-digits
  (lambda (n)
    (sum-of-digits-iter n 0))) ;börjar på 0, där result=0.

(define but-last-digit 
  (lambda (n)
    (remainder n 10)))

(define last-digit 
  (lambda (n)
    (quotient n 10)))
  
(define sum-of-digits-iter
  (lambda (n result)
     ;;För (but-last-digit a), där a är ett en-siffrigt tal returneras alltid 0. Därmed kan detta vara vår terminalfall.
    (if (= (but-last-digit n) 0) 
        ;; I slutet tas sista siffran + result. Result är summan av varje siffra som fås när last-digit anropas. 
        (+ result (last-digit n)) 
        ;; Så länge talet består av fler siffror än 1 görs beräknig. Alla siffror än det sista isoleras i varje beräkning, samt det sista
        ;värdet adderas med det "gamla result" för att bilda "det nya result"  
        (sum-of-digits-iter (but-last-digit n) (+ (last-digit n) result)))))
        

;;Hjälpproceduren "number-of-digits"

(define number-of-digits
  (lambda (n)
  ;; börjar på 0, vi lägger också till 1, för att but-last-digit (n)=0, när det är en siffra kvar att beräkna. 
    (+ 1 (number-of-digits-iter n 0)))) 

(define number-of-digits-iter
  (lambda (n counter)
    (if (= (but-last-digit n) 0)
        counter
        ;;ngt som räknar antalet varv som körs. Processen ska vara last  
        (number-of-digits-iter (but-last-digit n) (+ counter 1)))))

(define divisible?
  (lambda (n d)
    ; Det är endast delbart med ngt när resten är 0. 
    (if (= (remainder n d) 0)
        #t
        #f)))

(divisible? 3 4)


;;Hjälpproceduren "random-from-to"

(define random-from-to  
  (lambda (f t)
    (+ f (random t))))


;;UPPGIFT 7

;;Proceduren "simple-sv-num?" 

(define simple-sv-num?
  (lambda (n d)
  (if (= (remainder (sum-of-digits n) 10) 0)
      #t
      #f)))

;;Proceduren "make-simple-sv-num"
(define make-simple-sv-sum
  (lambda (n)
    (let ([num (random-from-to 100000 900000)])
    (if (= (remainder (sum-of-digits num) n) 0)
        num
        (make-simple-sv-sum n)))))

(make-simple-sv-sum 2)
(simple-sv-sum? (make-simple-sv-sum 2) 2)



















