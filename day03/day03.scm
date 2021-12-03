#!/usr/bin/env chibi-scheme

(import (scheme small)
        (scheme list)
        (scheme vector)
        (scheme bitwise))

;;> Reads input to list.

;;> Each line of the input is transformed into a string of bools
(define (read-input filename)
  (with-input-from-file filename
    (lambda ()
      (unfold eof-object?
              (lambda (line)
                (vector-map (lambda (b) (eqv? b #\1))
                            (string->vector line)))
              (lambda (_) (read-line))
              (read-line)))))

(define (part1 bits)
  (let-values (((gamma epsilon) (gamma-epsilon bits)))
    (* gamma epsilon)))

(define (gamma-epsilon rows)
  (let* ((majority (ceiling (/ (length rows) 2)))
         (counts (bitcounts rows))
         ; calculate a number where each bit is the one occurring most often in
         ; that column.
         (gamma (bitvec->int (vector-map
                               (lambda (count) (>= count majority))
                               counts)))
         ; one's complement of gamma
         (epsilon (- (expt 2 (vector-length (car rows)))
                     gamma 1)))

  (values gamma epsilon)))

;;> Calculates how many of the bits are 1 in each column
(define (bitcounts rows)
  (let ((rows (map (lambda (rows)
                     (vector-map (lambda (b) (if b 1 0)) rows))
                   rows)))
    (apply vector-map + rows)))

;;> Transforms a vector of bools into an integer
(define (bitvec->int v)
  (vector-fold (lambda (x b) (bitwise-ior (arithmetic-shift x 1)
                                          (if b 1 0)))
               0 v))

(define (part2 bits)
  (* (o2-generator-rating bits) (co2-scrubber-rating bits)))

(define (o2-generator-rating bits) (rating bits >=))
(define (co2-scrubber-rating bits) (rating bits <))

(define (rating rows cmp)
  (bitvec->int
    (let loop ((rows rows)
               ; bit to look at
               (i 0))
      (cond
        ((null-list? (cdr rows)) (car rows))        ; only one entry left
        ((>= i (vector-length (car rows))) (car rows))        ; all bits done
        (else
          (let* (; number of one-bits in the ith column
                 (count (fold (lambda (row sum) (if (vector-ref row i) (+ sum 1) sum))
                              0
                              rows))
                 ; find bit value to be kept
                 (to-be-kept (cmp count (- (length rows) count))))
            (loop
              (filter (lambda (b) (eqv? to-be-kept (vector-ref b i)))
                      rows)
              (+ i 1))))))))

(define bits (read-input (cadr (command-line))))
(write (part1 bits)) (newline)
(write (part2 bits)) (newline)
