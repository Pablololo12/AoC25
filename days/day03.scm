(define-module (days day03)
  #:export (run)
  #:use-module (srfi srfi-1))

(define (part1 line)
  (if (= (length line) 1)
      0
      (let ((values (map (lambda (x) (+ (* (car line) 10) x)) (cdr line))))
        (max (apply max values) (part1 (cdr line))))))

(define (indexed-max in)
  (letrec ((indx-m (lambda (l x m i)
                     (cond
                      ((null? l) (cons m i))
                      ((> (car l) m) (indx-m (cdr l) (+ 1 x) (car l) x))
                      (else (indx-m (cdr l) (+ 1 x) m i))))))
    (indx-m in 0 -1 -1)))

(define (part2r in left val)
  (cond
   ((= left 0) val)
   (else (let* ((searchList (take in (+ 1 (- (length in) left))))
                (localm (indexed-max searchList))
                (rest (drop in (+ 1 (cdr localm)))))
           (part2r rest (- left 1) (+ (car localm) (* 10 val)))))))

(define (part2 input)
  (part2r input 12 0))

(define (run input)
  (let ((in (map (lambda (x) (map (lambda (y) (- (char->integer y) (char->integer #\0))) (string->list x))) input)))
    (format #t "Part1: ~a~%Part2: ~a~%" (apply + (map part1 in)) (apply + (map part2 in)))))
