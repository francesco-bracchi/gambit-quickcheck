(##namespace ("quickcheck-strings#"))
(##include "~~/lib/gambit#.scm")

(define (just s #!key (weight 10))
  `((S ,s)))

(define (alt . as)
  (apply append (map rename as)))

(define (seq . as)
  (let((ps 
        (map (lambda (a)
               (let*((p (gensym 'P))
                     (a1 (replace 'S p (rename a))))
                 (cons p a1)))
             as)))
    `((S ,@(map car ps))
      ,@(apply append (map cdr ps)))))

(define (replace s0 s1 l)
  (let replace ((l l))
    (cond
     ((eq? l s0) s1)
     ((pair? l) 
      (cons (replace (car l))
            (replace (cdr l))))
     (else l))))
      
(define (rename grammar)       
  (let((table (make-table init: #f)))
    (map (rename-rule table) grammar)))

(define (rename-rule table)
  (lambda (rule)
    (map (rename-term table) rule)))

(define (rename-term table)
  (lambda (term)
    (cond
     ((string? term) term)
     ((number? term) term)
     ((eq? term 'S) 'S)
     (else
      (really-rename-term table term)))))

(define (really-rename-term table term)
  (let((res (table-ref table term #f)))
    (or res
        (let((s (gensym 'T)))
          (table-set! table term s)
          s))))

(define current-grammar (make-parameter #f))

(define (a-string #!key 
                  (grammar (current-grammar))
                  (cases 20))
  (generate! (string<< grammar: grammar
                       cases: cases)))

(define (string<< #!key 
                  (grammar (current-grammar))
                  (cases 20))
  (lambda (yield)
    (let loop ((j 0))
      (if (< j cases)
          (begin
            (yield (new-string grammar))
            (loop (+ j 1)))))))

(define (new-string grammar #!optional (state 'S))
  (let((rules (get-rules grammar state)))
    (apply-rule grammar 
                (list-ref rules (random-integer (length rules))))))

(define (get-rules grammar state)
  (filter (lambda (r) (eq? (car r) state))
          grammar))

(define (filter test? es)
  (let filter ((es es) (rs '()))
    (cond
     ((null? es) (reverse rs))
     ((test? (car es)) (filter (cdr es) (cons (car es) rs)))
     (else (filter (cdr es) rs)))))

(define (apply-rule grammar rule)
  (apply string-append
         (map (lambda (e) 
                (if (symbol? e) 
                    (new-string grammar e)
                    e))
              (cdr rule))))


(define-macro (define-grammar name)
;; see http://www-cs-faculty.stanford.edu/~zelenski/rsg/grammars/Haiku.g
  (##include "grammar.scm")
  `(define ,name 
     ,(list 'quote 
            (call-with-input-file 
                (string-append "grammars/" (symbol->string name) ".g")
              read-grammar))))
  

(define-grammar haiku)
(define-grammar kant)
(define-grammar insult)
      
(current-grammar kant)
(define all '())
(define reps '())

(time (test! 
 (let((string (a-string cases: 1)))
   (if (member string all) 
       (set! reps (cons string reps))
       (set! all (cons string all)))
   (display string)
   (newline))))

