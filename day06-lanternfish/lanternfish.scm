(import (scheme base)
        (scheme process-context)
        (scheme read)
        (scheme write)
        (scheme file)
        (scheme vector)
        (scheme regex))

(define (read-input filename)
  (with-input-from-file filename
    (lambda ()
      (map string->number (regexp-split '#\, (read-line))))))

(define (part1 spawn-cooldowns)
  (grow-for 80 spawn-cooldowns))

(define (part2 spawn-cooldowns)
  (grow-for 256 spawn-cooldowns))

(define (grow-for n spawn-cooldowns)
  (define fish-counts (spawn-cds->count-vector spawn-cooldowns))
  (do ((i 0 (+ i 1))
       (fish-counts fish-counts (grow fish-counts)))
      ((= i n) (vector-fold + 0 fish-counts))))

(define (spawn-cds->count-vector spawn-cooldowns)
  (define fish-counts (make-vector 9 0))
  (for-each (lambda (count)
              (vector-set! fish-counts count
                           (+ 1 (vector-ref fish-counts count))))
            spawn-cooldowns)
  fish-counts)

(define (grow fish-counts)
  (vector-unfold
    (lambda (i)
      (case i
        ((8) (vector-ref fish-counts 0))
        ((6) (+ (vector-ref fish-counts 0)
                (vector-ref fish-counts 7)))
        ((0 1 2 3 4 5 7) (vector-ref fish-counts (+ i 1)))))
    9))

(define spawn-cooldowns (read-input (cadr (command-line))))
(write (part1 spawn-cooldowns)) (newline)
(write (part2 spawn-cooldowns)) (newline)
