(import (scheme base)
        (scheme comparator)
        (scheme file)
        (scheme list)
        (scheme process-context)
        (scheme read)
        (scheme set)
        (scheme sort)
        (scheme vector)
        (scheme write))

(define-record-type <field>
  (make-field w h data)
  field?
  (w field-width)
  (h field-height)
  (data field-data))

(define (read-input filename)
  (with-input-from-file filename
    (lambda ()
      (let ((rows
              (unfold eof-object?
                      (lambda (line)
                        (list->vector
                          (map (lambda (c) (- (char->integer c) (char->integer #\0)))
                               (string->list line))))
                      (lambda (x) (read-line))
                      (read-line))))
        (make-field (vector-length (first rows))
                    (length rows)
                    (vector-concatenate rows))))))

(define (field-ref f i j)
  (if (or (< i 0) (>= i (field-height f))
          (< j 0) (>= j (field-width f)))
    10
    (vector-ref (field-data f) (+ j (* i (field-width f))))))

(define (field-set! f i j v)
  (if (and (>= i 0) (< i (field-height f))
           (>= j 0) (< j (field-width f)))
    (vector-set! (field-data f) (+ j (* i (field-width f))) v)))

(define (low-points f)
  (let ((res (set (make-default-comparator))))
    (do ((i 0 (+ i 1)))
        ((= i (field-height f)))
      (do ((j 0 (+ j 1)))
          ((= j (field-width f)))
        (if (and (< (field-ref f i j) (field-ref f (- i 1) j))
                 (< (field-ref f i j) (field-ref f (+ i 1) j))
                 (< (field-ref f i j) (field-ref f i (- j 1)))
                 (< (field-ref f i j) (field-ref f i (+ j 1))))
          (set-adjoin! res (cons i j)))))
    res))

(define (part1 field)
  (set-fold (lambda (p sum)
              (let-values (((i j) (car+cdr p)))
                (+ sum (field-ref field i j) 1)))
            0
            (low-points field)))

(define (neighbors i j)
  (list (cons (- i 1) j)
        (cons (+ i 1) j)
        (cons i (- j 1))
        (cons i (+ j 1))))

(define (basin-size field low-point)
  (let loop ((frontier (list low-point))
             (explored (set (make-default-comparator))))
    (if (null-list? frontier)
      (set-size explored)
      (let ((new
              (let ((i (caar frontier))
                    (j (cdar frontier)))
                (filter (lambda (neighbor)
                          (let-values (((ni nj) (car+cdr neighbor)))
                            (let ((val (field-ref field i j))
                                  (nval (field-ref field ni nj)))
                              (and (> nval val) (< nval 9)))))
                        (neighbors i j)))))
        (loop (lset-union equal? new (cdr frontier))
              (set-adjoin explored (car frontier)))))))

(define (part2 field)
  (fold * 1 (take (list-sort > (map (lambda (p) (basin-size field p))
                                    (set->list (low-points field))))
                  3)))

(define field (read-input (cadr (command-line))))
(write (part1 field)) (newline)
(write (part2 field)) (newline)

