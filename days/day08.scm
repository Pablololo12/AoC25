(define-module (days day08)
  #:export (run)
  #:use-module (srfi srfi-1))

(define (indexed-min in)
  (letrec ((indx-m (lambda (l x m i)
                     (cond
                      ((null? l) (cons m i))
                      ((< (car l) m) (indx-m (cdr l) (+ 1 x) (car l) x))
                      (else (indx-m (cdr l) (+ 1 x) m i))))))
    (indx-m in 0 +inf.0 0)))

(define (distance point1 point2)
  (apply + (map (lambda (x y) (expt (- x y) 2)) point1 point2)))

(define (equal point1 point2)
  (every = point1 point2))

(define (generate-pairs box)
  (let ((memo (make-hash-table)))
    (define (rec left)
      (cond
       ((null? (cdr left))
        (let* ((lh (hash-map->list (lambda (key val) (list key val)) memo))
               (lhsort (sort lh (lambda (a b) (< (car a) (car b))))))
          (map cadr lhsort)))
       (else
        (let* ((first (car left))
               (distances (filter identity (map (lambda (x) (if (not (equal x first)) (list (distance x first) first x) #f)) left))))
          (for-each (lambda (x) (hash-set! memo (car x) (cdr x))) distances)
          (rec (cdr left))))))
    (rec box)))

(define (point-in-group? p g)
  (any (lambda (x) (equal p x)) g))

(define (find-group p groups)
  (find (lambda (g) (point-in-group? p g)) groups))

(define (merge-pair pair groups)
  (let* ((a   (car pair))
         (b   (cadr pair))
         (ga  (find-group a groups))
         (gb  (find-group b groups))
         (rest (filter (lambda (g) (and (not (eq? g ga))
                                   (not (eq? g gb)))) groups)))
    (cond
      ((and ga gb (eq? ga gb))
       groups)
      ((and ga gb)
       (cons (lset-union equal? ga gb) rest))
      (ga (cons (cons b ga) rest))
      (gb (cons (cons a gb) rest))
      (else
       (cons (list a b) groups)))))

(define (merge groups pairs)
  (cond
   ((null? pairs) groups)
   (else
    (merge (merge-pair (car pairs) groups) (cdr pairs)))))

(define (merge-track boxes groups pairs)
  (cond
   ((null? pairs) "error")
   (else
    (let* ((pair (car pairs))
           (left (cdr pairs))
           (gg (merge-pair pair groups))
           (rr (remove (lambda (x) (equal x (car pair))) boxes))
           (rrr (remove (lambda (x) (equal x (cadr pair))) rr)))
      (cond
       ((and (null? rrr) (= (length gg) 1))
        (* (car (car pair)) (car (cadr pair))))
       (else
        (merge-track rrr gg left)))))))

(define (part1 input)
  (let* ((pairs (take (generate-pairs input) 1000))
         (merged (merge (list (car pairs)) (cdr pairs)))
         (groups (sort (map length merged) >)))
    (apply * (take groups 3))))

(define (part2 input)
  (let* ((pairs (generate-pairs input)))
    (merge-track input (list (car pairs)) (cdr pairs))))

(define (run input)
  (let* ((inn (map (lambda (x) (map string->number (string-split x #\,))) input)))
    (format #t "Part1: ~a~%Part2: ~a~%" (part1 inn) (part2 inn))))
