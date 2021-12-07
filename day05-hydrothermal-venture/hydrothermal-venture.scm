;;; --- Day 5: Hydrothermal Venture ---
;;;
;;; You come across a field of hydrothermal vents on the ocean floor! These
;;; vents constantly produce large, opaque clouds, so it would be best to avoid
;;; them if possible.
;;;
;;; They tend to form in lines; the submarine helpfully produces a list of
;;; nearby lines of vents (your puzzle input) for you to review. For example:
;;;
;;; 0,9 -> 5,9
;;; 8,0 -> 0,8
;;; 9,4 -> 3,4
;;; 2,2 -> 2,1
;;; 7,0 -> 7,4
;;; 6,4 -> 2,0
;;; 0,9 -> 2,9
;;; 3,4 -> 1,4
;;; 0,0 -> 8,8
;;; 5,5 -> 8,2
;;;
;;; Each line of vents is given as a line segment in the format x1,y1 -> x2,y2
;;; where x1,y1 are the coordinates of one end the line segment and x2,y2 are
;;; the coordinates of the other end. These line segments include the points at
;;; both ends. In other words:
;;;
;;;  -  An entry like 1,1 -> 1,3 covers points 1,1, 1,2, and 1,3.
;;;  -  An entry like 9,7 -> 7,7 covers points 9,7, 8,7, and 7,7.
;;;
;;; For now, only consider horizontal and vertical lines: lines where either x1
;;; = x2 or y1 = y2.
;;;
;;; So, the horizontal and vertical lines from the above list would produce the
;;; following diagram:
;;;
;;; .......1..
;;; ..1....1..
;;; ..1....1..
;;; .......1..
;;; .112111211
;;; ..........
;;; ..........
;;; ..........
;;; ..........
;;; 222111....
;;;
;;; In this diagram, the top left corner is 0,0 and the bottom right corner is
;;; 9,9. Each position is shown as the number of lines which cover that point
;;; or . if no line covers that point. The top-left pair of 1s, for example,
;;; comes from 2,2 -> 2,1; the very bottom row is formed by the overlapping
;;; lines 0,9 -> 5,9 and 0,9 -> 2,9.
;;;
;;; To avoid the most dangerous areas, you need to determine the number of
;;; points where at least two lines overlap. In the above example, this is
;;; anywhere in the diagram with a 2 or larger - a total of 5 points.
;;;
;;; Consider only horizontal and vertical lines. At how many points do at least
;;; two lines overlap?
;;;
;;; --- Part Two ---
;;;
;;; Unfortunately, considering only horizontal and vertical lines doesn't give
;;; you the full picture; you need to also consider diagonal lines.
;;;
;;; Because of the limits of the hydrothermal vent mapping system, the lines in
;;; your list will only ever be horizontal, vertical, or a diagonal line at
;;; exactly 45 degrees. In other words:
;;;
;;;  -  An entry like 1,1 -> 3,3 covers points 1,1, 2,2, and 3,3.
;;;  -  An entry like 9,7 -> 7,9 covers points 9,7, 8,8, and 7,9.
;;;
;;; Considering all lines from the above example would now produce the
;;; following diagram:
;;;
;;; 1.1....11.
;;; .111...2..
;;; ..2.1.111.
;;; ...1.2.2..
;;; .112313211
;;; ...1.2....
;;; ..1...1...
;;; .1.....1..
;;; 1.......1.
;;; 222111....
;;;
;;; You still need to determine the number of points where at least two lines
;;; overlap. In the above example, this is still anywhere in the diagram with a
;;; 2 or larger - now a total of 12 points.

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
