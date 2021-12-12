(import (scheme base)
        (scheme char)
        (scheme comparator)
        (scheme file)
        (scheme generator)
        (scheme list)
        (scheme hash-table)
        (scheme process-context)
        (scheme text)
        (scheme read)
        (scheme regex)
        (scheme set)
        (scheme write)
        (srfi 26))      ; cut/cute

(import (gauche vm debugger))

(define (read-input filename)
  (with-input-from-file filename
    (lambda ()
      (let ((connections (make-hash-table (make-default-comparator))))
        (generator-for-each (lambda (conn)
                              (let* ((parts (textual-split conn "-"))
                                     (a (first parts))
                                     (b (second parts)))
                                (hash-table-update!/default
                                  connections a (cute cons b <>) '())
                                (hash-table-update!/default
                                  connections b (cute cons a <>) '())))
                            read-line)
        connections))))

(define (small-cave? c)
  (char-lower-case? (string-ref c 0)))

(define (part1 connections)
  (let explore ((current "start")
                (visited (set (make-default-comparator))))
    (if (string=? current "end")
      1
      (let* ((visited (if (small-cave? current)
                        (set-adjoin visited current)
                        visited))
             (nexts (filter (lambda (c) (not (set-contains? visited c)))
                            (hash-table-ref connections current))))
        (fold + 0 (map (cute explore <> visited) nexts))))))

(define (part2 connections)
  (let explore ((current "start")
                (visited (set (make-default-comparator)))
                (visited-twice #f))
    (if (string=? current "end")
      1
      (let* ((visited (if (small-cave? current)
                        (set-adjoin visited current)
                        visited))
             (nexts (filter (lambda (c)
                              (or (and (not visited-twice)
                                       (not (string=? c "start")))
                                  (not (set-contains? visited c))))
                            (hash-table-ref connections current))))
        (fold + 0 (map (lambda (next)
                         (explore next 
                                  visited
                                  (or visited-twice (set-contains? visited next))))
                       nexts))))))

(define connections (read-input (cadr (command-line))))
(write (part1 connections)) (newline)
(write (part2 connections)) (newline)

