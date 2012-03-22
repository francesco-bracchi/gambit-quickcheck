(define (mergesort lt? es)
  
  (define (split es)
    (let split ((es es) (as '()) (bs '()))
      (if (null? es) (cons as bs)
          (split (cdr es)
                 bs
                 (cons (car es) as)))))
  
  (define (merge lt? as bs)
    (let merge ((as as) (bs bs) (rs '()))
      (cond
       ((and (null? as) (null? bs)) 
        (reverse rs))
       ((null? as) 
        (merge as (cdr bs) (cons (car bs) rs)))
       ((null? bs) 
        (merge (cdr as) bs (cons (car as) rs)))
       (else
        (let((a (car as))
             (b (car bs)))
          (if (lt? a b)
              (merge (cdr as) bs (cons a rs))
              (merge as (cdr bs) (cons b rs))))))))
  
  (let mergesort ((es es))
    (cond
     ((null? es) es)
     ((null? (cdr es)) es)
     (else (let*((ab (split es))
                 (a (mergesort  (car ab)))
                 (b (mergesort (cdr ab))))
             (merge lt? a b))))))
   
(define (quicksort lt? lst)
  
  (define (filter test? ls)
    (let filter ((ls ls) (rs '()))
      (cond
       ((null? ls) (reverse rs))
       ((test? (car ls)) (filter (cdr ls) (cons (car ls) rs)))
       (else (filter (cdr ls) rs)))))
  (let quicksort ((lst lst))
    (if (null? lst) '()
        (let*((pivot (car lst))
              (left (filter (lambda (j) (lt? j pivot)) (cdr lst)))
              (right (filter (lambda (j) (not (lt? j pivot))) (cdr lst))))
          (append
           (quicksort left)
           (list pivot)
           (quicksort right))))))
