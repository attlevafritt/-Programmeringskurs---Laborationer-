#| I detta dokument ska vi skriva alla klassdef. för sakerna/items |#
(require compatibility/mlist)
(provide item%)

(define item%
  (class object%
    (init-field
     [place #f]   
     [name "unknown object"]
     [description "despription: unknown"]
     
;; Klassens selektorer 
   (define/public (get-name)
     name)

    (define/public (get-description)
      description)

    (define/public (get-place)
      place) ;returnerar character%-objekt eller place%-objekt

    (define/public (move-item new-place)
      (set! place new-place))

    (super-new))))

;;Nya instanser av klassen

(define coffee-mug
  (new item%
       [place "Simme"] ;;MAYBE MODIFY
       [name "Coffee mug"]
       [description
        "This mug is the labasistant's favourite. The coffee inside always tastes especially good whenever
        Simme gets reminded of the fact that no one else but he gets free coffee. This item is really good at distracting people.
        Just like any other fragile objects once thrown on walls it breaks"]))


(define key
  (new item%
       ;;place specific room
       [name "key"]
       [description "key that opens the laboratory room"]))


(define liu-ID
  (new item%
       ;;place= sacraficial lamb 
       [name "liu-id"]
       [description "liu-id which is good for identification"]))






    

    

    
     
