;;; --- Day 9: Smoke Basin ---
;;;
;;; These caves seem to be lava tubes. Parts are even still volcanically
;;; active; small hydrothermal vents release smoke into the caves that slowly
;;; settles like rain.
;;;
;;; If you can model how the smoke flows through the caves, you might be able
;;; to avoid it and be that much safer. The submarine generates a heightmap of
;;; the floor of the nearby caves for you (your puzzle input).
;;;
;;; Smoke flows to the lowest point of the area it's in. For example, consider
;;; the following heightmap:
;;;
;;;     2199943210
;;;     3987894921
;;;     9856789892
;;;     8767896789
;;;     9899965678
;;;
;;; Each number corresponds to the height of a particular location, where 9 is
;;; the highest and 0 is the lowest a location can be.
;;;
;;; Your first goal is to find the low points - the locations that are lower
;;; than any of its adjacent locations. Most locations have four adjacent
;;; locations (up, down, left, and right); locations on the edge or corner of
;;; the map have three or two adjacent locations, respectively. (Diagonal
;;; locations do not count as adjacent.)
;;;
;;; In the above example, there are four low points, all highlighted: two are
;;; in the first row (a 1 and a 0), one is in the third row (a 5), and one is
;;; in the bottom row (also a 5). All other locations on the heightmap have
;;; some lower adjacent location, and so are not low points.
;;;
;;; The risk level of a low point is 1 plus its height. In the above example,
;;; the risk levels of the low points are 2, 1, 6, and 6. The sum of the risk
;;; levels of all low points in the heightmap is therefore 15.
;;;
;;; Find all of the low points on your heightmap. What is the sum of the risk
;;; levels of all low points on your heightmap?
;;;
;;;
;;; --- Part Two ---
;;;
;;; Next, you need to find the largest basins so you know what areas are most
;;; important to avoid.
;;;
;;; A basin is all locations that eventually flow downward to a single low
;;; point. Therefore, every low point has a basin, although some basins are
;;; very small. Locations of height 9 do not count as being in any basin, and
;;; all other locations will always be part of exactly one basin.
;;;
;;; The size of a basin is the number of locations within the basin, including
;;; the low point. The example above has four basins.
;;;
;;; The top-left basin, size 3:
;;;
;;;     2199943210
;;;     3987894921
;;;     9856789892
;;;     8767896789
;;;     9899965678
;;;
;;; The top-right basin, size 9:
;;;
;;;     2199943210
;;;     3987894921
;;;     9856789892
;;;     8767896789
;;;     9899965678
;;;
;;; The middle basin, size 14:
;;;
;;;     2199943210
;;;     3987894921
;;;     9856789892
;;;     8767896789
;;;     9899965678
;;;
;;; The bottom-right basin, size 9:
;;;
;;;     2199943210
;;;     3987894921
;;;     9856789892
;;;     8767896789
;;;     9899965678
;;;
;;; Find the three largest basins and multiply their sizes together. In the
;;; above example, this is 9 * 14 * 9 = 1134.
;;;
;;; What do you get if you multiply together the sizes of the three largest
;;; basins?


(import (scheme base)
        (scheme comparator)
        (scheme file)
        (scheme list)
        (scheme process-context)
        (scheme read)
        (scheme set)
        (scheme sort)
        (scheme vector)
        (scheme write))

(define-record-type <field>
  (make-field w h data)
  field?
  (w field-width)
  (h field-height)
  (data field-data))

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

(define (field-ref f i j)
  (if (or (< i 0) (>= i (field-height f))
          (< j 0) (>= j (field-width f)))
    10
    (vector-ref (field-data f) (+ j (* i (field-width f))))))

(define (field-set! f i j v)
  (if (and (>= i 0) (< i (field-height f))
           (>= j 0) (< j (field-width f)))
    (vector-set! (field-data f) (+ j (* i (field-width f))) v)))

(define (low-points f)
  (let ((res (set (make-default-comparator))))
    (do ((i 0 (+ i 1)))
        ((= i (field-height f)))
      (do ((j 0 (+ j 1)))
          ((= j (field-width f)))
        (if (and (< (field-ref f i j) (field-ref f (- i 1) j))
                 (< (field-ref f i j) (field-ref f (+ i 1) j))
                 (< (field-ref f i j) (field-ref f i (- j 1)))
                 (< (field-ref f i j) (field-ref f i (+ j 1))))
          (set-adjoin! res (cons i j)))))
    res))

(define (part1 field)
  (set-fold (lambda (p sum)
              (let-values (((i j) (car+cdr p)))
                (+ sum (field-ref field i j) 1)))
            0
            (low-points field)))

(define (neighbors i j)
  (list (cons (- i 1) j)
        (cons (+ i 1) j)
        (cons i (- j 1))
        (cons i (+ j 1))))

(define (basin-size field low-point)
  (let loop ((frontier (list low-point))
             (explored (set (make-default-comparator))))
    (if (null-list? frontier)
      (set-size explored)
      (let ((new
              (let ((i (caar frontier))
                    (j (cdar frontier)))
                (filter (lambda (neighbor)
                          (let-values (((ni nj) (car+cdr neighbor)))
                            (let ((val (field-ref field i j))
                                  (nval (field-ref field ni nj)))
                              (and (> nval val) (< nval 9)))))
                        (neighbors i j)))))
        (loop (lset-union equal? new (cdr frontier))
              (set-adjoin explored (car frontier)))))))

(define (part2 field)
  (fold * 1 (take (list-sort > (map (lambda (p) (basin-size field p))
                                    (set->list (low-points field))))
                  3)))

(define field (read-input (cadr (command-line))))
(write (part1 field)) (newline)
(write (part2 field)) (newline)

