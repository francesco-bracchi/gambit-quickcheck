(load "../src/quickcheck")
(include "../src/quickcheck#.scm")

(random-source-randomize! default-random-source)

(test! (let((a (a-string grammar: kant)))
         (display a)
	 (newline)))
(newline)

(test! (let((a (a-string))) ;; default kaiku
         (display a)
	 (newline)))
(newline)

(test! (let((a (a-string grammar: insult)))
         (display a)
	 (newline)))
(newline)