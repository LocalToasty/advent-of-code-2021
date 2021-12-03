#!/usr/bin/env chibi-scheme

(import (scheme small)
        (scheme list))

(define (read-input filename)
  (with-input-from-file filename
    (lambda ()
      (unfold eof-object?
              values
              (lambda (_) (read))
              (read)))))

(define (part1 depths)
  (count < depths (cdr depths)))

(define (part2 depths)
  (define window-sums (map + depths (cdr depths) (cddr depths)))
  (count < window-sums (cdr window-sums)))


(define depths (read-input (cadr (command-line))))
(write (part1 depths)) (newline)
(write (part2 depths)) (newline)
