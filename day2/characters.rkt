#lang racket

(define input
  (file->lines "input.txt"))

(define (count line)
  (for/fold ([h (make-hash)])
            ([c line])
    (hash-update! h c (lambda (val) (+ val 1)) 0)
    h))

(define (get-checksums)
  (call-with-values
   (lambda ()
     (for/fold ([twos 0]
                [threes 0])
               ([box input])
       (let ([outcome (count box)]
             [found-twos #f]
             [found-threes #f])
         (hash-for-each outcome
                        (lambda (letter occurences)
                          (cond
                            [(and (not found-twos) (= occurences 2))
                             (set! twos (add1 twos))
                             (set! found-twos #t)]
                            [(and (not found-threes) (= occurences 3))
                             (set! threes (add1 threes))
                             (set! found-threes #t)]))))
       (values twos threes))) *))

(define (get-similar corpus)
  (for* ([word corpus]
         [word2 corpus]
         #:unless (eq? word word2))
    (let ([result (for/fold ([diff-count 0]
                             #:result (if (= 1 diff-count) word null))
                            ([c word]
                             [c2 word2]
                             #:break (> diff-count 1))
                    (cond [(not (eq? c c2))
                           (add1 diff-count)]
                          [else diff-count]))])
      (when (not (null? result)) (displayln result)))))