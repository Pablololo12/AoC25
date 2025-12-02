(define-module (days day02)
  #:export (run)
  #:use-module (srfi srfi-1))

(define (getdig num)
  (inexact->exact (+ 1 (floor (log10 num)))))

(define (legal num)
  (let* ((ndig (getdig num))
         (half (expt 10 (quotient ndig 2))))
    (if (= (modulo ndig 2) 0)
        (if (= (modulo num half) (quotient num half))
              num
            0)
        0)))

(define (reccheck ac check group)
  (cond
   ((= ac 0) #t)
   ((= (modulo ac group) check) (reccheck (quotient ac group) check group))
   (else #f)))

(define (checkg rr num)
  (let ((group (expt 10 rr)))
    (reccheck (quotient num group) (modulo num group) group)))

(define (legal2 num)
  (let* ((ndig (getdig num))
         (rr (iota (quotient ndig 2) 1)))
    (if (any (lambda (x) 
               (and (= 0 (modulo ndig x)) 
                    (checkg x num))) rr)
        num
        0)))

(define (count-range range func)
  (let ((rr (iota (+ 1 (- (cadr range) (car range))) (car range))))
    (fold (lambda (x acum)
            (+ acum (func x))) 0 rr)))

(define (algo input func)
  (fold (lambda (x acum)
          (+ acum (count-range x func))) 0 input))

(define (run input)
  (let* ((spli (string-split (car input) #\,))
         (newin 
          (map (lambda (x)
                 (map string->number (string-split x #\-))) spli)))
    (format #t "Part1: ~a~%Part2: ~a~%" (algo newin legal) (algo newin legal2))))
