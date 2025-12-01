(define-module (days day01)
  #:export (run)
  #:use-module (srfi srfi-1))

(define (calcmove x y)
  (if (string=? "R" (car x))
      (+ y (cdr x))
      (- y (cdr x))))

(define (reccalc x y)
  (if (and (> x -1) (< x 100))
      (if (= x 0)
          (cons x 1)
          (cons x 0))
      (cond
       ((< x 0) (reccalc (+ 100 x) y))
       (else (reccalc (- x 100) y)))))

(define (reccalc2 x y)
  (if (and (> x -1) (< x 100))
      (cons x y)
      (cond
       ((< x 0) (reccalc2 (+ 100 x) (+ y 1)))
       (else (reccalc2 (- x 100) (+ y 1))))))

(define (calcjump x y)
  ;; x is the instruction and y the position
  (let ((mov (calcmove x y)))
    (reccalc mov 0)))

(define (calcjump2 x y)
  ;; x is the instruction and y the position
  (let ((mov (calcmove x y)))
    (reccalc2 mov 0)))

(define (part1 input)
  (fold (lambda (x acum)
          (let ((n (calcjump x (car acum))))
            ;;(format #t "~a ~a~%" x acum)
            (cons (car n) (+ (cdr n) (cdr acum)))))
           (cons 50 0) input))

(define (part2 input)
  (fold (lambda (x acum)
          (let ((n (calcjump2 x (car acum))))
            (format #t "~a ~a~%" x acum)
            (cons (car n) (+ (cdr n) (cdr acum)))))
           (cons 50 0) input))

(define (run input)
  (let* ((rows (map (lambda (x)
                      (cons
                       (substring x 0 1)
                       (string->number
                        (substring x 1 (string-length x))))) input)))
    (display "Part1: ")
    (display (part1 rows))
    (newline)
    (display "Part2: ")
    (display (part2 rows))
    (newline)))
