(define-module (days day06)
  #:export (run)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 rdelim))


(define (split-on-spaces s)
  (filter (lambda (x) (not (string-null? x)))
          (string-split (string-trim-both s) #\space)))

(define (part1 input acc)
  (if (null? (car input))
      acc
      (let* ((first (map car input))
            (rest (map cdr input))
            (op (if (string=? (car first) "*") * +))
            (nums (map string->number (cdr first))))
        (part1 rest (+ acc (apply op nums))))))

(define (get-num inp)
  (let ((digits (reverse (map car inp))))
    (let loop ((acc 0)
               (ll digits))
      (cond
       ((null? ll) acc)
       ((char=? (car ll) #\space) (loop acc (cdr ll)))
       (else (loop (+ (* acc 10) (- (char->integer (car ll)) (char->integer #\0))) (cdr ll)))))))

(define (filter-zero in)
  (filter (lambda (x) (> x 0)) in))

(define (part2 input)
  (let ((mod (reverse (map reverse (map string->list input)))))
    (let loop ((left mod)
               (acc 0)
               (numlist '()))
      (if (null? (car left))
        acc
        (let* ((fc (car (car left)))
               (opp (or (char=? fc #\*) (char=? fc #\+)))
               (op (if (char=? fc #\*) * +))
               (num (get-num (cdr left))))
          (if opp
              (loop (map cdr left) (+ acc (apply op (filter-zero (cons num numlist)))) '())
              (loop (map cdr left) acc (cons num numlist))))))))

(define (run input)
  (let ((mod (reverse (map (lambda (x) (split-on-spaces x)) input))))
    (format #t "Part1: ~a~%Part2: ~a~%" (part1 mod 0) (part2 input))))
