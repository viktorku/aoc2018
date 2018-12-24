#lang racket

(define (recurring-freq lines)
  (for/fold ([acc (cons 0 (set))]
             #:result (car acc))
            ([freq (in-cycle lines)]
             #:break (set-member? (cdr acc) (car acc)))
    (cons (+ (car acc) freq) (set-add (cdr acc) (car acc)))))

(call-with-input-file "input.txt"
  (lambda (f)
    (let ([lines (map string->number (sequence->list (in-lines f)))])
      (printf "Resulting frequency: ~a\n" (foldl + 0 lines))
      (printf "First recurring frequency result: ~a\n" (recurring-freq lines)))))
