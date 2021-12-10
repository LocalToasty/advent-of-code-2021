(import (scheme base)
        (scheme read)
	(scheme write)
        (scheme file)
        (scheme text)
        (scheme list)
        (scheme process-context)
        (gauche vm debugger))

(define (read-input filename)
  (with-input-from-file filename
    (lambda ()
      (define tokens (textual-split (read-line) ","))
      (map (lambda (number-text)
             (read (open-input-string (textual->string number-text))))
           tokens))))

(define (part1 positions)
  (grad-desc cost positions))

(define (cost positions target)
  (fold (lambda (pos sum) (+ sum (abs (- pos target)))) 0 positions))

;; gradient descent with fixed step size of 1
(define (grad-desc cost positions)
  (let* ((start (floor-quotient (fold + 0 positions) (length positions)))
	 (grad (- (cost positions (+ start 1))
		  (cost positions start)))
	 (dir (if (> grad 0) -1 1)))
    (let loop ((target start)
               (best-cost (cost positions start)))
      (let* ((new-target (+ target dir))
             (new-cost (cost positions new-target)))
        (if (< new-cost best-cost)
          (loop new-target new-cost)
          best-cost)))))

(define (part2 positions)
  (grad-desc cost2 positions))

(define (cost2 positions target)
  (fold (lambda (pos sum) (+ sum (sumto (abs (- pos target))))) 0 positions))

;; sum of integers from 1 to n
(define (sumto n)
  (/ (* n (+ n 1)) 2))

(define positions (read-input (cadr (command-line))))

(write (part1 positions)) (newline)
(write (part2 positions)) (newline)
