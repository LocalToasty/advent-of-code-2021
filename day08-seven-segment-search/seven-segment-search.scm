(import (scheme base)
        (scheme process-context)
        (scheme read)
        (scheme write)
        (scheme file)
        (scheme regex)
        (scheme list)
        (scheme mapping)
        (scheme comparator)
        (scheme sort))

(define (read-input filename)
  (with-input-from-file filename
    (lambda ()
      (unfold eof-object?
              parse-datapoint
              (lambda (x) (read-line))
              (read-line)))))

(define comp (make-default-comparator))

(define (parse-datapoint line)
  (let* ((segments (regexp-extract '(+ (/ "ag")) line))
         (segment-chars (map (lambda (s) (list-sort char<? (string->list s))) segments))
         (segment-symbols (map (lambda (x) (map (lambda (c) (string->symbol (string c)))
                                                x))
                               segment-chars)))
    (call-with-values (lambda () (split-at segment-symbols 10))
                      cons)))

(define (part1 inputs)
  (let ((unique-count? (lambda (x) (case (length x)
                                     ((2 3 4 7) #t)
                                     (else #f)))))
    (fold (lambda (line sum)
            (+ sum
               (count unique-count? (cdr line))))
            0
            inputs)))

;; calculates all the max-length-elem subset permutations of elems 
(define (permutations elems max-length)
  (if (= max-length 1)
    (map list elems)
    (concatenate (map (lambda (first)
                        (map (lambda (rest) (cons first rest))
                               (permutations (delete first elems)
                                             (- max-length 1))))
                        elems))))

;; creates a mapping from a list of keys and values
(define (lists->mapping keys vals)
  (mapping-unfold (lambda (kvs) (null-list? (car kvs)))
                  (lambda (kvs) (values (caar kvs)
                                        (cadr kvs)))
                  (lambda (kvs) (cons (cdar kvs)
                                      (cddr kvs)))
                  (cons keys vals)
                  comp))


;; generates all mappings between two sets
(define (mappings-from-to from to)
  (let ((perms (permutations to (length to))))
    (map (lambda (perm) (lists->mapping from perm))
         perms)))


;; all possible 7-segment mappings
(define all-mappings (mappings-from-to '(a b c d e f g)
                                       '(a b c d e f g)))

;; a mapping from numbers to 7-segment encodings
(define num->sevseg
  (mapping comp
           0 '(a b c e f g)
           1 '(c f)
           2 '(a c d e g)
           3 '(a c d f g)
           4 '(b c d f)
           5 '(a b d f g)
           6 '(a b d e f g)
           7 '(a c f)
           8 '(a b c d e f g)
           9 '(a b c d f g)))

;; a mapping from (sorted) 7-segment encodings to numbers
(define sevseg->num
  (mapping-map (lambda (k v) (values v k))
               comp
               num->sevseg))

(define (mappings-from from)
  (concatenate (map (lambda (n) (%mappings-to-n from n))
                   (case (length from)
                     ((2) '(1))
                     ((3) '(7))
                     ((4) '(4))
                     ((5) '(2 3 5))
                     ((6) '(0 6 9))
                     ((7) '(8))))))

;; all possible mappings from an encoding to a number
(define (%mappings-to-n from n)
  (mappings-from-to from (mapping-ref num->sevseg n)))

(define (>=any? m1 . ms)
  (any (lambda (m) (mapping>=? comp m1 m))
       ms))

(define (sort-symbol-list l)
  (map string->symbol (list-sort string<? (map symbol->string l))))

;; finds a mapping which decodes the scrambled digits
(define (find-mapping inputs)
  (define mapping (fold (lambda (input mappings)
                          (filter (lambda (m)
                                    (apply >=any? m (mappings-from input)))
                                  mappings))
                        all-mappings
                        (list-sort (lambda ls (apply < (map length ls))) inputs)))
  (car mapping))

;; decodes a single number
(define (decode-scrambled-digit mapping scramble)
  (mapping-ref sevseg->num
               (sort-symbol-list (map (lambda (x)
                                        (mapping-ref mapping x))
                                      scramble))))


;; converts a scrambled sequence into a number
(define (decode-scrambled-sequence mapping sequence)
    (fold (lambda (x sum) (+ (* sum 10) (decode-scrambled-digit mapping x)))
          0
          sequence))

(define (part2 inputs)
  (define decoded
    (map (lambda (row)
          (let-values (((train test) (car+cdr row)))
            (let ((mapping (find-mapping train)))
              (decode-scrambled-sequence mapping test))))
         inputs))
  (fold + 0 decoded))

(define inputs (read-input (cadr (command-line))))
(write (part1 inputs)) (newline)
(write (part2 inputs)) (newline)
