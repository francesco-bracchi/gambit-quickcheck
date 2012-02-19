(define (in-set<< lst)
  (lambda (yield)
    (for-each yield lst)))

;; catenate 2 generators
(define (alt<< . gs)
  (lambda (yield)
    (for-each (lambda (g) (g yield)) gs)))

(define (in-set vs)
  (generate! (in-set<< vs)))

(define (any-of . vs) (in-set vs))
