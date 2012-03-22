(load "../src/quickcheck")
(include "../src/quickcheck#.scm")

(include "sort.scm")

(random-source-randomize! default-random-source)

(define (a-random-list #!key (cases 100))
  (let((len (in-range stop: cases)))
    (let loop ((j 0) (rs '()))
      (if (>= j len) rs
          (loop (+ j 1) (cons (random-integer (* 10 len))
                              rs))))))

(define (test-sort fn)
  (let*((unord (or (a-random-list cases: 100)
                   (any-of '(0))))
        (ord (fn < unord)))
    (assert! (all? (lambda (x) (member x ord)) unord))
    (assert! (all? (lambda (x) (member x unord)) ord))
    (let*((len (length ord))
	  (i (in-range stop: len))
	  (j (in-range stop: len)))
      (assert! (=> (>= i j)
		   (>= (list-ref ord i) 
		       (list-ref ord j)))))))

(test! (test-sort mergesort))
