(define-module (days day05)
  #:export (run)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11))

(define (part1 range ing)
  (apply + (map (lambda (x)
                  (if (any (lambda (y)
                             (and (<= (car y) x) (>= (cdr y) x))) range)
                      1
                      0))
                ing)))

(define (merge-element full rest)
  (let* ((new     (car rest))
         (merged  (map (lambda (x) (check-merge x new)) full))
         (changed (any (lambda (x) (ranges-overlap? x new)) full)))
    (if changed
        merged
        (cons new full))))

(define (merge start lis)
  (if (nil? lis)
      start
      (merge (merge-element start lis) (cdr lis))))

(define (ranges-overlap? r1 r2)
    (and (<= (car r1) (cdr r2))
         (<= (car r2) (cdr r1))))

(define (check-merge r1 r2)
  (if (ranges-overlap? r1 r2)
      (cons (min (car r1) (car r2))
            (max (cdr r1) (cdr r2)))
      r1))

(define (part2 lis)
  (let loop ((prev (length lis))
             (new lis))
    (let ((nn (merge (cons (car new) '()) (cdr new))))
      (if (= (length nn) prev)
          (apply + (map (lambda (x) (+ (- (cdr x) (car x)) 1)) nn))
          (loop (length nn) nn)))))

(define (run input)
  (let-values (((range ingredients) 
                (break (lambda (x) (not (string-index x #\-))) input)))
    (let ((rng (map (lambda (x)
                      (let ((sp (string-split x #\-)))
                        (cons (string->number (car sp)) (string->number (cadr sp))))) range))
          (ing (map string->number ingredients)))
      (format #t "Part1: ~a~%Part2: ~a~%" (part1 rng ing) (part2 rng)))))
