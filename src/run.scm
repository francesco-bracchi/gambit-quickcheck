(define-structure quickcheck-exception message)

(define (generate! fn)
  (call/cc 
   (lambda (ret) 
     (fn ret))))
     
(call/gen 
 (lambda (generate!_)
   (set! generate! generate!_))
 on-fail: (lambda () (raise "not found")))


(define (_run-test fn #!key 
                  (max-branches 100)
                  (max-depth 100))
  (let((_generate! generate!))
    (call/gen
     (lambda (generate!_)
       (dynamic-wind
           (lambda () (set! generate! generate!_))
           (lambda () (fn) (generate! nothing))
           (lambda () (set! generate! _generate!))))
     max-branches: max-branches
     max-depth: max-depth
     on-fail: (lambda () 'success))))


(define (run-test fn #!key 
                  (max-branches 100)
                  (max-depth 100)
                  (ids #f))
  (if ids 
      (_run-test-ids fn max-branches: max-branches max-depth: max-depth)
      (_run-test fn max-branches: max-branches max-depth: max-depth)))

(define (_run-test-ids fn #!key 
                       (max-branches 100)
                       (max-depth 100))
  (let run ((i 1) (j 1))
    (_run-test fn max-branches: i max-depth: j)
    (run (+ i 1) (+ j 1))))

(define (assert expr message)
  (if (not expr) (raise (make-quickcheck-exception message))))

(define (nothing y) 'nothing)
  