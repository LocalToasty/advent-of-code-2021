#!/usr/bin/env chibi-scheme

(import (scheme small)
        (scheme list)
        (scheme vector))

(define (read-input filename)
  (with-input-from-file filename
    (lambda ()
      (unfold (lambda (inst) (not inst))
              (lambda (x) x)
              (lambda (_) (read-instruction))
              (read-instruction)))))

(define (read-instruction . in)
  (let* ((direction (apply read in))
         (amount (apply read in)))
    (if (eof-object? amount)
        #f
        (cons direction amount))))

(define (part1 instrs)
  (let loop ((instrs instrs)
             (depth 0)
             (pos 0))
    (if (null-list? instrs)
        (* depth pos)
        (let ((instr (car instrs)))
          (let ((dir (car instr))
                (amt (cdr instr)))
            (cond ((eqv? dir 'forward) (loop (cdr instrs) depth (+ pos amt)))
                  ((eqv? dir 'down)    (loop (cdr instrs) (+ depth amt) pos))
                  ((eqv? dir 'up)      (loop (cdr instrs) (- depth amt) pos))
                  (else (error "illegal command" dir))))))))

(define (part2 instrs)
  (let loop ((instrs instrs)
             (aim 0)
             (depth 0)
             (pos 0))
    (if (null-list? instrs)
        (* depth pos)
        (let ((instr (car instrs)))
          (let ((dir (car instr))
                (amt (cdr instr)))
            (cond ((eqv? dir 'forward) (loop (cdr instrs) aim         (+ depth (* aim amt)) (+ pos amt)))
                  ((eqv? dir 'down)    (loop (cdr instrs) (+ aim amt) depth                 pos))
                  ((eqv? dir 'up)      (loop (cdr instrs) (- aim amt) depth                 pos))
                  (else (error "illegal command" dir))))))))

(define instructions (read-input (cadr (command-line))))
(write (part1 instructions)) (newline)
(write (part2 instructions)) (newline)
