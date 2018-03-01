(define (null-ld? obj)
  (if (not (pair? obj)) #f
    (eq? (car obj) (cdr obj))))

(define (listdiff? obj)
  (cond
    ((null-ld? obj) #t)
    ((not (pair? obj)) #f)
    ((eq? (car obj) (cdr obj)) #t)
    ((not (pair? (car obj))) #f)
    (#t (listdiff? (cons (cdr (car obj)) (cdr obj))))))


(define (cons-ld obj listdiff)
  (cons (cons obj (car listdiff)) (cdr listdiff)))

(define (car-ld listdiff)
  (if (and (not (null-ld? listdiff)) (listdiff? listdiff))
	(car (car listdiff))
        (display "error\n")))

(define (cdr-ld listdiff)
  (if (and (not (null-ld? listdiff)) (listdiff? listdiff))
      (cons (cdr (car listdiff)) (cdr listdiff))
        (display "error\n")))

(define (listdiff . obj) (cons obj '()))

(define (length-ld listdiff)
  (cond
    ((not (listdiff? listdiff)) (display "error\n"))
    (else
      (let ((cdrld (cdr listdiff)))
        (let count ((listd (car listdiff)))
          (if
            (eq? listd cdrld) 0
            (+ 1 (count (cdr listd)))))))))

(define (append-ld . ldif_other)
  (cond
    ((null? (cdr ldif_other)) (car ldif_other))
    (#t
     (let ((new_list (apply append-ld (cdr ldif_other))))
       (cons (append (listdiff->list (car ldif_other)) (car new_list)) (cdr new_list))))))

(define (list-tail-ld listdiff k)
	(cond
		((> k (length-ld listdiff)) (display "error\n"))
    ((= k 0) listdiff)
		  (else
			 (list-tail-ld (cons (cdr (car listdiff)) (cdr listdiff)) (- k 1)))))

(define (list->listdiff list)
  (if (not (list? list))
      (display "error\n")
      (apply listdiff (car list) (cdr list))))

(define (listdiff->list listdiff)
  (cond
    ((eq? (car listdiff) (cdr listdiff)) '())
    (#t (cons (car (car listdiff)) (listdiff->list (cons (cdr (car listdiff)) (cdr listdiff)))))))

(define (expr-returning listdiff)
    (if (not (listdiff? listdiff))
    (display "error\n")
    `(cons ',(take (car listdiff) (length-ld listdiff)) '())))    
  
;--------------------------------------------------

;(define ils (append '(a e i o u) 'y))
;(define d1 (cons ils (cdr (cdr ils))))
;(define d2 (cons ils ils))
;(define d3 (cons ils (append '(a e i o u) 'y)))
;(define d4 (cons '() ils))
;(define d5 0)
;(define d6 (listdiff ils d1 37))
;(define d7 (append-ld d1 d2 d6))
;(define e1 (expr-returning d1))

;d1
;d2
;d3
;d4
;d5
;d6
;d7
;e1
;(display "---null-ld\n")
;(null-ld? d1)
;(null-ld? d2)
;(null-ld? d3)
;(null-ld? d6)
;(display "---listdiff?\n")
;(listdiff? d1)
;(listdiff? d2)
;(listdiff? d3)
;(listdiff? d4)
;(listdiff? d5)
;(listdiff? d6)
;(listdiff? d7)
;(display "---car-ld\n")
;(car-ld d1)
;(car-ld d2)
;(car-ld d3)
;(car-ld d6)
;(display "---length-ld\n")
;(length-ld d1)
;(length-ld d2)
;(length-ld d3)
;(length-ld d6)
;(length-ld d7)

;(define kv1 (cons d1 'a))
;(define kv2 (cons d2 'b))
;(define kv3 (cons d3 'c))
;(define kv4 (cons d1 'd))
;(define d8 (listdiff kv1 kv2 kv3 kv4))
;(define d9 (listdiff kv3 kv4))


;(display "----eq? d8\n")
;(eq? d8 (list-tail-ld d8 0))           
;(equal? (listdiff->list (list-tail-ld d8 2))
 ;       (listdiff->list d9))           
;(null-ld? (list-tail-ld d8 4))         
;(list-tail-ld d8 -1)                  
;(list-tail-ld d8 5)                    

;(display "----eq? (car\n")
;(eq? (car-ld d6) ils)                 
;(eq? (car-ld (cdr-ld d6)) d1)          
;(eqv? (car-ld (cdr-ld (cdr-ld d6))) 37)
;(equal? (listdiff->list d6)
;        (list ils d1 37))             
;(eq? (list-tail (car d6) 3) (cdr d6))  

;(listdiff->list (eval e1))             
;(equal? (listdiff->list (eval e1))
;        (listdiff->list d1))          