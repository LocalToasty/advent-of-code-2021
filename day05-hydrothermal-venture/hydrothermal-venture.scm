(import (scheme base)
	(scheme process-context)
	(scheme read)
	(scheme write)
	(scheme file)
	(scheme cxr)
        (scheme list)
        (scheme vector)
        (scheme hash-table)
	(scheme comparator)
        (scheme regex))

(define-record-type <line>
  (make-line p1 p2)
  line?
  (p1 p1) (p2 p2))

(define (read-input filename)
  (with-input-from-file filename
    (lambda ()
      (unfold eof-object?
              parse-line
              (lambda (x) (read-line))
              (read-line)))))


(define (parse-line line)
  (define numbers (map (lambda (n) (read (open-input-string n)))
                       (regexp-extract '(+ numeric) line)))

  (make-line (vector (car numbers)
                     (cadr numbers))
             (vector (caddr numbers)
                     (cadddr numbers))))

(define (x1 l) (vector-ref (p1 l) 0))
(define (y1 l) (vector-ref (p1 l) 1))
(define (x2 l) (vector-ref (p2 l) 0))
(define (y2 l) (vector-ref (p2 l) 1))

(define (horizontal-or-vertical? line)
  (or (= (x1 line) (x2 line))
      (= (y1 line) (y2 line))))

(define (part1 lines)
  (define world (make-hash-table (lambda (x y) (vector= = x y))))
  (for-each
    (lambda (line)
      (if (horizontal-or-vertical? line)
        (draw world line)))
    lines)
  (hash-table-fold (lambda (k v sum) (if (>= v 2) (+ sum 1) sum))
                   0 world))

(define (draw world line)
  (let* ((step (vector- (p2 line) (p1 line)))
         (absmax (vector-fold (lambda (m x) (if (> (abs x) m) (abs x) m))
                              1 step))
         (step (vector-map (lambda (x) (round (/ x absmax)))
                           step)))
    (let ((start (p1 line))
          (end (vector+ (p2 line) step)))
      (do ((p start (vector+ p step)))
          ((vector= = p end))
        (hash-table-update!/default world p (lambda (x) (+ x 1)) 0)))))

(define (vector+ . vs)
  (apply vector-map + vs))

(define (vector- . vs)
  (apply vector-map - vs))

(define (part2 lines)
  (define world (make-hash-table (make-default-comparator)))
  (for-each (lambda (line) (draw world line)) lines)
  (hash-table-fold (lambda (k v sum) (if (>= v 2) (+ sum 1) sum))
                   0 world))

(define lines (read-input (cadr (command-line))))
(write (part1 lines)) (newline)
(write (part2 lines)) (newline)
