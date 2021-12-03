#!/usr/bin/env chibi-scheme

(import (scheme small)
        (scheme list)
        (scheme vector)
        (scheme bitwise))

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

(define (gamma-epsilon bits)
  (let* ((majority (ceiling (/ (length bits) 2)))
	 (counts (bitcounts bits))
         (gamma (bitvec->int (vector-map
                               (lambda (count) (>= count majority))
                               counts)))
         (epsilon (- (expt 2 (vector-length (car bits)))
                     gamma 1)))
  (values gamma epsilon)))

(define (bitcounts bits)
  (let ((bits (map (lambda (bits) (vector-map (lambda (b) (if b 1 0)) bits))
                   bits)))
    (apply vector-map + bits)))

(define (bitvec->int v)
  (vector-fold (lambda (x b) (bitwise-ior (arithmetic-shift x 1)
                                          (if b 1 0)))
               0 v))

(define (part2 bits)
  (* (o2-generator-rating bits) (co2-scrubber-rating bits)))

(define (o2-generator-rating bits) (rating bits >=))
(define (co2-scrubber-rating bits) (rating bits <))

(define (rating bits cmp)
  (bitvec->int
    (let loop ((bits bits)
               (i 0))
      (cond ((null-list? (cdr bits)) (car bits))
            ((>= i (vector-length (car bits))) (car bits))
            (else
        (let* ((count (vector-ref (bitcounts bits) i))
               (most-common (cmp count
                                 (- (length bits) count))))
          (loop
            (filter (lambda (b) (eqv? (vector-ref b i)
                                      most-common))
                    bits)
            (+ i 1))))))))

(define bits (read-input (cadr (command-line))))
(write (part1 bits)) (newline)
(write (part2 bits)) (newline)
