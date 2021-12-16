(import (scheme base)
        (scheme file)
        (scheme process-context)
        (scheme read)
        (scheme write)
        (scheme comparator)
        (scheme set)
        (scheme list)
        (scheme vector)
        (scheme sort))

(define-record-type <array2d>
  (make-array2d w h data)
  array2d?
  (w array2d-width)
  (h array2d-height)
  (data array2d-data))

(define (array2d-ref f i j)
  (if (or (< i 0) (>= i (array2d-height f))
          (< j 0) (>= j (array2d-width f)))
    10
    (vector-ref (array2d-data f) (+ j (* i (array2d-width f))))))

(define (array2d-set! f i j v)
  (if (and (>= i 0) (< i (array2d-height f))
           (>= j 0) (< j (array2d-width f)))
    (vector-set! (array2d-data f) (+ j (* i (array2d-width f))) v)))

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
        (make-array2d (vector-length (first rows))
                      (length rows)
                      (vector-concatenate rows))))))

(define-record-type <location>
  (location pos cost)
  location?
  (pos pos)
  (cost cost))

(define (expansions chiton current)
  (define res '())
  (define-values (i j) (car+cdr (pos current)))
  (define (add i j)
    (set! res (cons (location (cons i j)
                              (+ (cost current) (array2d-ref chiton i j)))
                    res)))
  (if (> i 0) (add (- i 1) j))
  (if (< i (- (array2d-height chiton) 1)) (add (+ i 1) j))
  (if (> j 0) (add i (- j 1)))
  (if (< j (- (array2d-width chiton) 1)) (add i (+ j 1)))
  res)

(define (find-way chiton)
  (define goal (cons (- (array2d-height chiton) 1)
                     (- (array2d-width chiton) 1)))
  (define (loc<? x y)
    (< (cost x) (cost y)))
  (let loop ((frontier (list (location (cons 0 0) 0)))
             (explored (set (make-default-comparator))))
    (let ((current (car frontier)))
      (cond ((equal? (pos current) goal) (cost current))
            ((set-contains? explored (pos current)) (loop (cdr frontier) explored))
            (else
              (let ((nexts (filter (lambda (loc) (not (set-contains? explored (pos loc))))
                                   (expansions chiton current))))
                (loop (list-merge! loc<? (cdr frontier)
                                   (list-sort! loc<? nexts))
                      (set-adjoin! explored (pos current)))))))))

(define part1 find-way)

(define (part2 chiton)
  (find-way (replicate chiton 5)))

(define (replicate chiton n)
  (define old-w (array2d-width chiton))
  (define old-h (array2d-height chiton))
  (define new-w (* n old-w))
  (define new-h (* n old-h))
  (make-array2d new-w
                new-h
                (vector-unfold
                  (lambda (idx)
                    (define-values (i j) (floor/ idx new-w))
                    (let-values (((n i) (floor/ i old-h))
                                 ((m j) (floor/ j old-w)))
                      (+ (modulo (+ (vector-ref (array2d-data chiton)
                                                (+ j (* i old-h)))
                                    n m -1)
                                 9)
                         1)))
                  (* new-w new-h))))

(define chiton (read-input (cadr (command-line))))
(write (part1 chiton)) (newline)
(write (part2 chiton)) (newline)
