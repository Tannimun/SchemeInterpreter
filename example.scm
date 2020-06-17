(define lat?
    (lambda {l}
        (cond
            {(null? l) #t}
            {(atom? (car l)) (lat? (cdr l))}
            {else #f})))

(lat? {bacon and eggs})

(define member? 
    (lambda {a lat} 
        (cond 
            {(null? lat) #f} 
            {else (or (eq? (car lat) a) 
                    (member? a (cdr lat)))}))) 

(member? meat {mashed potatoes and meat gravy})

(define members?
    (lambda {a lat}
        (cond
            {(null? lat) #f})))