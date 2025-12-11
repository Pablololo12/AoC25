(define-module (days day09)
  #:export (run))

(define (get-area p1 p2)
  (* (abs (- (car p1) (car p2))) (abs (- (cadr p1) (cadr p2)))))

(define (max))

(define (part1 input)
  "todo")

(define (part2 input)
  "todo")

(define (run input)
  (let ((inn (map (lambda (x) (map string->number (string-split x #\,))) input)))
    (format #t "Part1: ~a~%Part2: ~a~%" (part1 inn) (part2 inn))))
