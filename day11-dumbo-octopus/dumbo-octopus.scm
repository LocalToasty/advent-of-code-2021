(import (scheme base)
        (scheme file)
        (scheme list)
        (scheme process-context)
        (scheme read)
        (scheme vector)
        (scheme write))

(define-record-type <field>
  (make-field w h data)
  field?
  (w field-width)
  (h field-height)
  (data field-data))

(define (field-ref f i j)
  (if (or (< i 0) (>= i (field-height f))
          (< j 0) (>= j (field-width f)))
    10
    (vector-ref (field-data f) (+ j (* i (field-width f))))))

(define (field-set! f i j v)
  (if (and (>= i 0) (< i (field-height f))
           (>= j 0) (< j (field-width f)))
    (vector-set! (field-data f) (+ j (* i (field-width f))) v)))

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

(define (charge-octopus! octopi i j)
  (call/cc
    (lambda (return)
      (if (or (< i 0) (>= i (field-height octopi))
              (< j 0) (>= j (field-width octopi))
              (> (field-ref octopi i j) 9))
        (return))

      (field-set! octopi i j (+ 1 (field-ref octopi i j)))
      (if (= (field-ref octopi i j) 10)
        (for-each (lambda (i2)
                    (for-each (lambda (j2) (if (< (field-ref octopi i2 j2) 10)
                                             (charge-octopus! octopi i2 j2)))
                              (iota 3 (- j 1))))
                  (iota 3 (- i 1)))))))

(define (part1 octopi)
  (let ((octopi (make-field (field-width octopi) (field-height octopi)
                            (vector-copy (field-data octopi)))))
    (call/cc
      (lambda (return)
        (let loop ((n 100)
                   (flash-sum 0))
        (if (= n 0)
          (return flash-sum))

        (charge-all! octopi)
        (let ((flash-count (vector-count (lambda (x) (> x 9))
                                         (field-data octopi))))
          (vector-map! (lambda (x) (if (> x 9) 0 x)) (field-data octopi))
          (loop (- n 1)
                (+ flash-sum flash-count))))))))

(define (charge-all! octopi)
  (for-each (lambda (i)
              (for-each (lambda (j) (charge-octopus! octopi i j))
                        (iota (field-width octopi))))
            (iota (field-height octopi))))

(define (part2 octopi)
  (let ((octopi (make-field (field-width octopi) (field-height octopi)
                            (vector-copy (field-data octopi)))))
    (call/cc
      (lambda (return)
        (let loop ((iter 0))
          (if (vector-every (lambda (x) (= x 0)) (field-data octopi))
            (return iter))
          (charge-all! octopi)
          (vector-map! (lambda (x) (if (> x 9) 0 x)) (field-data octopi))
          (loop (+ iter 1)))))))

(define octopi (read-input (cadr (command-line))))
(write (part1 octopi)) (newline)
(write (part2 octopi)) (newline)
