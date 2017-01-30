#lang racket
;;UPPGIFT 1

(define foo (* 1 2 3 4))
(define foobar (lambda () (* 1 2 3 4)))

#|SVAR: "foo" är en variabel som har ett värde på 24, den returnerar
därmed 24. Det är ingen procedur och kan därmed inte 
evalueras som ett (d.v.s. "(foo)" ger error). 
foobar är en definierad procedure, 
som tar in ett tomt argument, men har inget
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

;En iterativ lösning.
(define sum-of-digits
  (lambda (n)
    ;Beräkningen börjar på 0, d result= 0
    (sum-of-digits-iter n 0)))   

;;Hjälpproceduren "last-digit"
(define last-digit 
  (lambda (n)
    ;;Resten från delning med 10 blir vår sista siffra.
    (remainder n 10)))

;;Hjälpproceduren "but-last-digit" 
(define but-last-digit 
  (lambda (n)
    ;Detta ger allt förutom den sista siffran. 
    (quotient n 10)))
  
;;Hjälpproceduren "sum-of-digit-iter" 
(define sum-of-digits-iter
  (lambda (n result)
     ;;För (but-last-digit a), där a är ett en-siffrigt tal returneras alltid 0. Därmed kan detta vara vår terminalfall.
    (if (= (but-last-digit n) 0) 
        ;; I slutet tas sista siffran + "result". "result" är summan av varje siffra som fås när last-digit anropas. 
        (+ result (last-digit n))
        ;; Så länge talet består av fler siffror än 1 görs beräknig. Alla siffror än det sista isoleras i varje beräkning, samt det sista
        ;värdet adderas med det "gamla result" för att bilda "det nya result"  
        (sum-of-digits-iter (but-last-digit n) (+ (last-digit n) result)))))
        

;;Hjälpproceduren "number-of-digits"
(define number-of-digits
  (lambda (n)
  ;;Vi börjar beräkningen på 0, vi lägger också till 1, för att (but-last-digit n) = 0, när det är en siffra kvar att beräkna. 
    (+ 1 (number-of-digits-iter n 0)))) 

(define number-of-digits-iter
  (lambda (n counter)
    (if (= (but-last-digit n) 0)
        counter
        ;;ngt som räknar antalet varv som körs. Processen ska vara last  
        (number-of-digits-iter (but-last-digit n) (+ counter 1)))))

;; Hjälpproceduren "divisible?" 
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

;En funktion som returnerar 2*siffran(i det sex-siffriga talet) 
(define double
  (lambda (n)
    (+ n n)))

;Huvudproceduren som heter "make-cc-sv-num" .
(define make-cc-sv-num 
  (lambda ()
    ;låt "stored" vara det genererande 6-siffriga talet. Denna variabel binder ett talet som "random-from-to" genererar. 
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
  ;"n" är det sex-siffriga talet, "sum-of-all-viktade-tal-but-first" är summan av de viktade siffrorna "i" talet (förutom det första), 
  ;;"i" håller koll på varannat tal (om det är jämnt eller ej med andra ord). "i" börjar på 0.
  (lambda (n sum-of-all-viktade-tal-but-first i)
    ;När det sista talet räknas så returneras "sum-of-all-viktade-tal-but-first". 
    (if (= (but-last-digit n) 0)
       sum-of-all-viktade-tal-but-first
        ;räkning börjar på det nästa talet (med position 2, det första talet ignoreras med andra ord).
        (if (= i 1)
            ;proceduren anropas igen med uppdaterad värde på argumenten.
            (sum-of-digits-cc (but-last-digit n) (+ (last-digit n) sum-of-all-viktade-tal-but-first) 0)
            ;i=1 tolkas som att det är en jämn siffra. 
            (if (< (last-digit n) 5)
                (sum-of-digits-cc (but-last-digit n) (+ (double (last-digit n)) sum-of-all-viktade-tal-but-first) 1)
                (sum-of-digits-cc (but-last-digit n) (+ (double (last-digit n)) 1 sum-of-all-viktade-tal-but-first) 1))))))


;;UPPGIFT 9

;;DEL 9A

(define sum-and-apply-to-digits
  ;;Funktionen tar ett tal samt en procedur "digit-proc"
  (lambda (n digit-proc)
    ;;Terminalfallet, när sista siffran evalueras är (number-of-digits n) = 1   
    (if (= (number-of-digits n) 1) 
        (digit-proc n 1)
        (+ (digit-proc (last-digit n) (number-of-digits n))
           (sum-and-apply-to-digits (but-last-digit n) digit-proc)))))

;;DEL 9B

;"number-of-digits-high-order" 

;;En procedur för "number-of-digits-high-order". Varje position i ett tal ska endast returnera 1. 
(define just-count-the-positions
  (lambda (num pos)
  ;; Varje siffras position kommer endast returnera 1 (genom att vi delar med positionen) summan av dessa är antalet siffror i talet. 
  (+ (/ pos pos))))

(define number-of-digits-high-order
  (lambda (number)
    (sum-and-apply-to-digits number just-count-the-positions)))

;;"sum-of-digits-high-order"

;; En procedur som enast returnerar varje enskild siffra i talet.   
(define summering
  (lambda (number pos)
    number)) 

(define sum-of-digits-high-order
  (lambda (number)
    ;varje enskild siffra som returneras kommer att summeras ihop via "sum-and-apply-to-digits"
    (sum-and-apply-to-digits number summering)))

;;"make-cc-sv-num-high-order"

;;En procedur till "make-cc-sv-num-high-order"
(define count-viktade-summa 
  (lambda (number pos)
    ;Om positionen är jämn. 
    (if (divisible? pos 2)
      ;; Om numret är större eller lika med 5, ska siffran dubbleras samt adderas med 1. Annars dubbleras den endast.  
      (if (>= number 5)
          (+ 1 (double number))
          (double number))
    number)))

(define make-cc-sv-num-high-order 
  (lambda ()
    ;number binds till ett slumpmässigt genererat tal. 
    (let ([number (random-from-to 100000 999999)])
      ;;Talet måste vara delbart med 10,annars måste ett nytt tal genereras. 
      (if (divisible? (sum-and-apply-to-digits number count-viktade-summa) 10)
          number
          (make-cc-sv-num-high-order)))))

      
;; UPPGIFT 10 
#|Uppgiften nedan har gjorts enligt Luhn-algoritmen, se anvisningar i wikipedia.org: 
https://sv.wikipedia.org/wiki/Personnummer_i_Sverige#Kontrollsiffran(2017-01-30)
|#

;Varannnat siffra i personnummret ska multipliceras med 2 och 1. Sedan ska allt summeras (notera att varje siffra ska summeras) 
;;Det börjar med att det första (därmed alla tal med "udda" position) talet multipliceras med 2.  
(define 2-or-1
  (lambda (number pos)
    ;;varannan siffra multipliceras med 2, börjar från det andra siffran i talet. 
    (if (divisible? pos 2)
        number
        ;;Vid dubblering kan ett två-siffrigt tal skapas, därmed tar vi "sum-of-digits"
        (sum-of-digits (double number)))))
    
(define person-number?
  (lambda (person.nr)
   ;;Personnumret verifieras till sant om siffersumman på allt plus kontrollsiffran är delbart med 10. 
   (if (divisible? (+ (last-digit person.nr) (sum-and-apply-to-digits (but-last-digit person.nr) 2-or-1)) 10)
       #t
       #f)))
                       
