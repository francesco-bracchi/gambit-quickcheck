(load "../src/quickcheck")
(include "../src/quickcheck#.scm")

(random-source-randomize! default-random-source)

(define table (make-vector 20 0))
(define cases 100000)
(define _sum 0)

(define (test-mean)
  (let*((val (+ 9.5 (* 18 (a-normal term-count: 5 cases: cases))))
        (j  (inexact->exact (floor val)))
        (j (min (max j 0) 19)))
    (vector-set! table j (+ 1 (vector-ref table j)))
    (set! _sum (+ _sum val))))
    

(define (test-exponential)
  (let*((val (an-exponential mean: 100 cases: cases))
        (j  (inexact->exact (floor val))))
    (if (< val 20)
        (vector-set! table j (+ 1 (vector-ref table j))))
    (set! _sum (+ _sum val))))
           
(pp (test! (test-mean)))

(define (show-bar len)
  (let show ((len len))
    (if (<= len 1) (newline)
        (begin
          (display "#")
          (show (- len 1))))))

(define (test fn)
  (let((mx '())
       (gamma '()))

    (pp (test! (fn)))
    
    (set! mx (apply max (vector->list table)))
    
    (set! gamma (/ 150 mx))
    
    (let loop ((j 0))
      (if (>= j (vector-length table)) 'ok
	  (begin
	    (show-bar (* gamma (vector-ref table j)))
	    (loop (+ j 1)))))
    
    
    (pp (/ _sum cases))
    (pp table)))

(test test-mean)

(test test-exponential)
