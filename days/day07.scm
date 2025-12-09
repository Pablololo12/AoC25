(define-module (days day07)
  #:export (run)
  #:use-module (srfi srfi-1))

(define (get-start input)
  (cons 1 (list-index (lambda (x) (char=? x #\S)) (string->list (car input)))))

(define (get-index input)
  (let* ((mod (string->list input))
         (ids (iota (length mod)))
         (z (zip ids mod)))
    (map car (filter (lambda (x) (char=? (cadr x) #\^)) z))))

(define (get-splitter input acc i)
  (cond
   ((null? input) acc)
   (else (get-splitter
          (cdr input)
          (append (map (lambda (x) (cons i x)) (get-index (car input))) acc)
          (+ i 1)))))

(define (pair-exists? p lst)
  (any (lambda (x)
          (and (equal? (car x) (car p))
               (equal? (cdr x) (cdr p))))
        lst))

(define (dfs open visited splitters limit)
  (cond
   ((null? open) visited)
   (else
    (let* ((new (car open))
           (removed (cdr open))
           (next (cons (+ 1 (car new)) (cdr new))))
      (cond
       ((pair-exists? next splitters)
        (let* ((left (cons (car next) (- (cdr next) 1)))
               (right (cons (car next) (+ (cdr next) 1)))
               (le (pair-exists? left visited))
               (re (pair-exists? right visited)))
          (cond
           ((and le re) (dfs removed visited splitters))
           ((and le (not re)) (dfs (cons right removed) (cons right visited) splitters limit))
           ((and (not le) re) (dfs (cons left removed) (cons left visited) splitters limit))
           (else
            (dfs (cons left (cons right removed)) (cons left (cons right visited)) splitters limit)))))
       ((pair-exists? next visited) (dfs removed visited splitters limit))
       ((>= (car new) limit) (dfs removed visited splitters limit))
       (else
        (dfs (cons next removed) (cons next visited) splitters limit)))))))

(define (print-row row width splitters visited)
  (let loop ((i 0))
    (cond
     ((= i width) (newline))
     ((pair-exists? (cons row i) splitters)
      (display "^") (loop (+ i 1)))
     ((pair-exists? (cons row i) visited)
      (display "|") (loop (+ i 1)))
     (else
      (display ".") (loop (+ i 1))))))

(define (print-tree start end width splitters visited)
  (cond
   ((= start end) #t)
   (else
    (print-row start width splitters visited)
    (print-tree (+ start 1) end width splitters visited))))

(define (part1 visited splitter)
  (apply + (map (lambda (x)
                  (if (pair-exists? (cons (- (car x) 1) (cdr x)) visited) 1 0)) splitter)))

(define (part2 input width start)
  (let ((vec (make-vector width 0)))
    (vector-set! vec (cdr start) 1)
    (let loop ((lines input))
      (cond
       ((null? lines)
        (fold + 0 (vector->list vec)))
       (else
        (let inner ((i 0))
          (when (< i width)
            (when (and (> (vector-ref vec i) 0)
                       (char=? (string-ref (car lines) i) #\^))
              (vector-set! vec (- i 1)
                           (+ (vector-ref vec i)
                              (vector-ref vec (- i 1))))
              (vector-set! vec (+ i 1)
                           (+ (vector-ref vec i)
                              (vector-ref vec (+ i 1))))
              (vector-set! vec i 0))
            (inner (+ i 1))))
        (loop (cdr lines)))))))

(define (run input)
  (let* ((start (get-start input))
         (splitter (get-splitter input '() 0))
         (visited (dfs (cons start '()) (cons start '()) splitter (length input))))
;;    (print-tree 0 (length input) (string-length (car input)) splitter visited)
    (format #t "Part1: ~a~%Part2: ~a~%"
            (part1 visited splitter)
            (part2 input (string-length (car input)) start))))
