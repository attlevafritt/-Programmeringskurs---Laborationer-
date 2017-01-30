#lang racket

(provide (all-defined-out))
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
(define sum-iter
  (lambda (n)
    (sum-iter-counter n 0)))

(define sum-iter-counter
  (lambda (n result)
    #|Räkningen följer substitionsmetoden och 
      uppdaterar resultatet tills n=0|#
    (if (= n 0) 
        result
        (sum-iter-counter (- n 1)
                  (+ n result)))))

;;UPPGIFT 3

#|Anledningen till att det inte fungerar är att labass
har använt "expt" som ett argument. Det han egentligen 
vill ådstakomma är en liknande funktion som den definierade
nedan (se vår lösning)|#


(define powers-of-two
  (lambda (n)
    (expt 2 n)))

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

(define last-digit 
  (lambda (n)
    (remainder n 10)))

(define but-last-digit 
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


;;Hjälpproceduren "random-from-to"

(define random-from-to  
  (lambda (f t)
    (+ f (random (- t (- f 1))))))

;;UPPGIFT 7

;;Proceduren "simple-sv-num?" 
(define simple-sv-num?
  (lambda (n d)
    (if (divisible? (sum-of-digits n) d)
      #t
      #f)))


;;Proceduren "make-simple-sv-num"

(define make-simple-sv-num 
  (lambda (d)
    (let ([num (random-from-to 100000 999999)]) 
      (if (divisible? (sum-of-digits num) d)
        num
        (make-simple-sv-num d)))))





;UPPGIFT 8 
;Låt i vara antalet varv som körs när följande kod körs. 

;(define make-cc-sv-num  (lambda ()
    ;Vi behöver sats för Vi. 
    ;Vi behöver kontroll av i genom antal varv. 
    ;Om vi har ett i som kontrollerar om den är jämn eller inte. En variabel som växlar mellan 1 och 0. Låter i=1 vara jämnt tal. i=0 ursprung. (if (= i 1)|#

 
;En funktion som returnerar 2*siffran(i det sex-siffriga talet) 
(define double
  (lambda (n)
    (+ n n)))

;Huvudkoden som har funktionen "make-cc-sv-num" .
(define make-cc-sv-num 
  (lambda ()
    ;låt stored vara det genererande 6-siffriga talet.
    (let ([stored (random-from-to 100000 999999)]) 
        (if (divisible? 
        ;om summan av de viktade talen är delbart med 10 returneras det sex-siffriga-talet. Annars ska ett nytt tal genereras. 
                (+ (sum-of-digits-cc stored 0 0) 
                   ;quotient lägger till det första talet, som är ett udda tal.
                   (quotient stored 100000))
                10)
                stored
            (make-cc-sv-num)))))


(define sum-of-digits-cc
  ;n är det sex-siffriga talet, sum-to-next-last är summan av de viktade siffrorna i talet (förutom det första), i håller koll på varannat tal (om det är jämnt eller ej). i börjar på 0.
  (lambda (n sum-to-next-last i)
    ;När det sista talet räknas så returneras sum-to-next-last.
    (if (= (but-last-digit n) 0)
        sum-to-next-last
        ;räkning börjar på det nästa talet (med position 2).
        (if (= i 1)
            ;funk. anropas igen med uppdaterad värde på argumenten.Förkortar från höger till vänster. 
            (sum-of-digits-cc (but-last-digit n) (+ (last-digit n) sum-to-next-last) 0)
            ;i=1 tolkas som att det är en jämn siffra. 
            (if (< (last-digit n) 5)
                (sum-of-digits-cc (but-last-digit n) (+ (double (last-digit n)) sum-to-next-last) 1)
                (sum-of-digits-cc (but-last-digit n) (+ (double (last-digit n)) 1 sum-to-next-last) 1))))))


;;UPPGIFT 9

;;DEL 9A

(define sum-and-apply-to-digits
  ;;Funktionen tar ett tal samt en procedur "digit-proc"
  (lambda (n digit-proc)
    ;;Terminalfallet, när sista siffran evalueras ska "digit-proc" returneras.  
    (if (= (number-of-digits n) 1) 
        (digit-proc n 1)
        (+ (digit-proc (last-digit n) (number-of-digits n))
           (sum-and-apply-to-digits (but-last-digit n) digit-proc)))))

;;DEL 9B

;number-of-digits-high-order TESTNING OK! 
(define just-count-the-positions
  (lambda (num pos)
  (+ (/ pos pos))))

(define number-of-digits-high-order
  (lambda (number)
    (sum-and-apply-to-digits number just-count-the-positions)))

;sum-of-digits-high-order Testning OK!
(define summering
  (lambda (number pos)
    number)) 

(define sum-of-digits-high-order
  (lambda (number)
    (sum-and-apply-to-digits number summering)))

(define count-viktade-summa 
  (lambda (number pos)
    (if (divisible? pos 2)
      (if (>= number 5)
          (+ 1 (double number))
          (double number))
    number)))

;;make-cc-sv-num-high-order Testning OK! 
(define make-cc-sv-num-high-order 
  (lambda ()
    (let ([number (random-from-to 100000 999999)])
      (if (divisible? (sum-and-apply-to-digits number count-viktade-summa) 10)
          number
          (make-cc-sv-num-high-order)))))

      
;; UPPGIFT 10
;;Generera 10-siffrigt tal

;En procedur som
(define 2-or-1
  (lambda (number pos)
    (if (divisible? pos 2)
        number
        (sum-of-digits (double number)))))
    
(define person-number?
  (lambda (person.nr)
   (if (divisible? (+ (last-digit person.nr) (sum-and-apply-to-digits (but-last-digit person.nr) 2-or-1)) 10)
       #t
       #f)))
