(import (scheme base)
        (scheme comparator)
        (scheme file)
        (scheme list)
        (scheme process-context)
        (scheme read)
        (scheme hash-table)
        (scheme sort)
        (scheme write))

(define matching (hash-table (make-default-comparator)
                             #\( #\)
                             #\[ #\]
                             #\{ #\}
                             #\< #\>))

(define error-scores (hash-table (make-default-comparator)
                                 #\) 3
                                 #\] 57
                                 #\} 1197
                                 #\> 25137))

(define (read-input filename)
  (with-input-from-file filename
    (lambda ()
      (unfold eof-object?
              string->list
              (lambda (x) (read-line))
              (read-line)))))

(define (find-error line)
  (call/cc
    (lambda (break)
      (let loop ((remaining line)
                 (opened '()))
        (if (null-list? remaining)
          (if (null-list? opened) #f opened)
          (let ((curr (car remaining))
                (rest (cdr remaining)))
            (case (car remaining)
              ((#\( #\[ #\{ #\<) (loop rest (cons curr opened)))
              (else (if (eqv? curr (hash-table-ref matching (car opened)))
                      (loop (cdr remaining) (cdr opened))
                      (break curr))))))))))

(define (part1 lines)
  (fold + 0
        (map (lambda (c) (hash-table-ref/default error-scores c 0))
             (map find-error lines))))

(define completion-scores (hash-table (make-default-comparator)
                                      #\( 1 #\[ 2 #\{ 3 #\< 4))

(define (part2 lines)
  (let ((scores
          (map (lambda (remaining)
                 (fold (lambda (c sum) (+ (* sum 5)
                                          (hash-table-ref completion-scores c)))
                       0
                       remaining))
                (filter list? (map find-error lines)))))
    (list-ref (list-sort < scores) (floor-quotient (length scores) 2))))

(define lines (read-input (cadr (command-line))))
(write (part1 lines)) (newline)
(write (part2 lines)) (newline)
