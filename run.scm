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
           (lambda () (fn) (nothing))
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

(define-macro (test! e #!key 
                  (max-branches +inf.0)
                  (max-depth +inf.0)
                  (ids #f))
  `(run-test (lambda () ,e)
             max-branches: ,max-branches
             max-depth: ,max-depth
             ids: ,ids))

(define-macro (assert! expr #!optional (message (list 'quote `(fail! ,expr))))
  `(assert ,expr ,message))

(define (nothing) (generate! (lambda (yield) 'nothing)))
