(load "../src/quickcheck")
(include "../src/quickcheck#.scm")

(define (fail)
  (let((a (any-of 0 2 4 5 6 10 12 13 14 16 18 20 21)))
    (assert! (even? a))))

(test! (fail))
         