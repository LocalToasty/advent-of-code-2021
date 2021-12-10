(import (scheme base)
	(scheme process-context)
	(scheme file)
	(scheme read)
	(scheme write)
        (scheme list)
        (scheme vector)
        (scheme text)
        (scheme bitwise))

(define-record-type <board>
  (make-board numbers marks)
  board?
  (numbers numbers)
  (marks marks))

(define (read-input filename)
  (with-input-from-file filename
    (lambda ()
      (define tokens (textual-split (read-line) ","))
      (define called-numbers
        (map (lambda (number-text)
               (read (open-input-string (textual->string number-text))))
             tokens))
      (define boards (unfold eof-object?
                             values
                             (lambda (x) (read-board))
                             (read-board)))
      (values called-numbers boards))))

(define (read-board . in)
  (define numbers (vector-unfold (lambda (i) (apply read in)) 25))
  (if (eof-object? (vector-ref numbers 24))
    (eof-object)
    (make-board numbers (make-vector 25 #f))))

;;> Returns the board with the number marked
(define (mark-number board number)
  (make-board (numbers board)
         (vector-map (lambda (mark x)
                       (or mark (= x number)))
                     (marks board)
                     (numbers board))))

;;> Get a row of the board
(define (board-row board i)
  (vector-copy (marks board) (* 5 i) (* 5 (+ i 1))))

(define (board-column board j)
  (vector-unfold (lambda (i)(vector-ref (marks board) (+ (* 5 i) j)))
                 5))

;;> Get a column of the board
(define (bingo? board)
  (or (any values
           (map (lambda (i) (vector-every values (board-row board i)))
                (iota 5)))
      (any values
           (map (lambda (j) (vector-every values (board-column board j)))
                (iota 5)))))

(define (part1 calls boards)
  (call-with-values
    (lambda ()
      (let loop ((calls calls)
                 (boards boards))
        (let* ((boards (map (lambda (b) (mark-number b (car calls)))
                            boards))
               (bingo-board (find bingo? boards)))
          (if bingo-board
            (values bingo-board (car calls))
            (loop (cdr calls) boards)))))
    score))

(define (score board last-call)
  (* last-call
     (vector-fold (lambda (sum n m) (if (not m) (+ sum n) sum))
                  0
                  (numbers board)
                  (marks board))))

(define (part2 calls boards)
  (let loop ((calls calls)
             (boards boards))
    (let* ((boards (map (lambda (b) (mark-number b (car calls)))
                        boards))
           ; filter out all losing boards
           (losing-boards (filter (lambda (b) (not (bingo? b)))
                                  boards)))
      (if (null-list? (cdr losing-boards))
        (part1 (cdr calls) losing-boards)       ; play out the rest of the game normally
        (loop (cdr calls) losing-boards)))))

(define-values (called-numbers boards) (read-input (cadr (command-line))))
(write (part1 called-numbers boards)) (newline)
(write (part2 called-numbers boards)) (newline)
