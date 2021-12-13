(import (scheme base)
        (scheme file)
        (scheme process-context)
        (scheme read)
        (scheme write)
        (scheme comparator)
        (scheme generator)
        (scheme list)
        (scheme set)
        (scheme regex))

(define (read-input filename)
  (with-input-from-file filename
    (lambda ()
      (define (read-coords)
        (let ((match (regexp-search '(: ($(+ num)) #\, ($(+ num)))
                                    (read-line))))
          (and match (cons (string->number (regexp-match-submatch match 1))
                           (string->number (regexp-match-submatch match 2))))))
      (let* ((paper (set-unfold (make-default-comparator)
                                not
                                values
                                (lambda (x) (read-coords))
                                (read-coords)))
             (instructions
               (generator-map->list
                 (lambda (instr)
                   (let ((match (regexp-search '(: ($(/ "xy")) #\= ($(+ num)))
                                               instr)))
                     (cons (string->symbol (regexp-match-submatch match 1))
                           (string->number (regexp-match-submatch match 2)))))
                 read-line)))
        (values paper instructions)))))

(define (print-paper paper)
  (let ((max-x (set-fold (lambda (coord m) (max m (car coord)))
                         0 paper))
        (max-y (set-fold (lambda (coord m) (max m (cdr coord)))
                         0 paper)))
    (do ((y 0 (+ y 1)))
        ((> y max-y))
      (do ((x 0 (+ x 1)))
          ((> x max-x))
        (display (if (set-contains? paper (cons x y)) #\# #\ )))
      (newline))))

(define (fold-up paper pos)
  (set-map (make-default-comparator)
           (lambda (coord)
             (define-values (x y) (car+cdr coord))
             (cons x
                   (if (< y pos) y (- (* 2 pos) y))))
           paper))

(define (fold-left paper pos)
  (set-map (make-default-comparator)
           (lambda (coord)
             (define-values (x y) (car+cdr coord))
             (cons (if (< x pos) x (- (* 2 pos) x))
                   y))
           paper))

(define (fold-paper instr paper)
  (define-values (axis pos) (car+cdr instr))
  (cond ((symbol=? 'y axis) (fold-up paper pos))
        ((symbol=? 'x axis) (fold-left paper pos))))

(define count-dots set-size)

(define (part1 paper instructions)
  (count-dots (fold-paper (first instructions) paper)))

(define (part2 paper instructions)
  (print-paper (fold fold-paper paper instructions)))

(define-values (paper instructions) (read-input (cadr (command-line))))
(write (part1 paper instructions)) (newline)
(part2 paper instructions)
