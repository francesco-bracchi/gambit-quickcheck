(namespace 
 ("quickcheck#"
  call/gen
  run-test
  generate!
  test!
  ;; assert!
  =>
  <=>

  in-set<<
  alt<<
  in-set
  any-of

  ;; reals
  quantile:normal
  quantile:exponential
  quantile:uniform

  real<<
  normal<<
  exponential<<

  a-real
  a-normal
  an-exponential

  ;; integers
  in-range
  an-integer

  ;; strings
  current-grammar
  string<<
  a-string
  haiku
  kant
  insult
  ))
  
(define-macro (test! e #!key 
                  (max-branches +inf.0)
                  (max-depth +inf.0)
                  (ids #f))
  `(run-test (lambda () ,e)
             max-branches: ,max-branches
             max-depth: ,max-depth
             ids: ,ids))

;; logical implication
(define-macro (=> p q)
  `(or (not ,p) ,q))

;; logical if and only if
(define-macro (<=> p q)
  (let((p0 (gensym 'p))
       (q0 (gensym 'q)))
    `(let((,p0 p)
          (,q0 q))
       (and (or ,p0 (not ,q0))
            (or ,q0 (not ,p0))))))
