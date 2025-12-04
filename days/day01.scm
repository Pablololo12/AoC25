(define-module (days day01)
  #:export (run)
  #:use-module (srfi srfi-1))

(define (calcmove x y)
  (if (string=? "R" (car x))
      (+ y (cdr x))
      (- y (cdr x))))

(define (reccalc x y)
  (if (and (> x -1) (< x 100))
      (cons x (if (= x 0) 1 0))
      (reccalc ((if (< x 0) + -) x 100) (+ y 1))))

(define (calcjump x y)
  (let ((mov (calcmove x y)))
    (reccalc mov 0)))

(define (reccalc2 x y dir)
  (if (and (> x -1) (< x 100))
      (cons x (if (= x 0) (+ dir y) y))
      (reccalc2 ((if (< x 0) + -) x 100) (+ y 1) dir)))

(define (calcjump2 x y)
  (let* ((mov (calcmove x y))
         (count (if (and (= y 0) (< mov 0)) -1 0))
         (dir (if (> mov 0) 0 1)))
    (reccalc2 mov count dir)))

(define (algo input func)
  (cdr (fold (lambda (x acum)
          (let ((n (func x (car acum))))
                        (cons (car n) (+ (cdr n) (cdr acum)))))
           (cons 50 0) input)))

(define (run input)
  (let* ((rows (map (lambda (x)
                      (cons
                       (substring x 0 1)
                       (string->number
                        (substring x 1 (string-length x))))) input)))
    (format #t "Part1: ~a~%Part2: ~a~%" (algo rows calcjump) (algo rows calcjump2))))
