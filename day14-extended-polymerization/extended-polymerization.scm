(import (scheme base)
        (scheme file)
        (scheme process-context)
        (scheme read)
        (scheme write)
        (scheme comparator)
        (scheme list)
        (scheme hash-table)
        (scheme regex))

(define (read-input filename)
  (with-input-from-file filename
    (lambda ()
      (let* ((template (string->list (read-line)))
             (_ (read-line))
             (rules (hash-table-unfold
                      eof-object?
                      (lambda (line)
                        (let ((parts (map (lambda (str) (string-ref str 0))
                                          (regexp-extract '(/ #\A #\Z) line))))
                          (values (cons (first parts) (second parts))
                                  (third parts))))
                      (lambda (x) (read-line))
                      (read-line)
                      (make-default-comparator))))
        (values template rules)))))

(define (apply-rules template rule)
  (fold-right (lambda (x xs)
                (if (null-list? xs)
                  (cons x xs)
                  (cons* x
                         (hash-table-ref rules (cons x (car xs)))
                         xs)))
              '()
              template))

(define (part1 template rules)
  (define-values (min-count max-count)
    (min-max-count (counts template 10)))
  (- max-count min-count))

(define (part1 template rules)
  (define-values (min-count max-count)
    (min-max-count (counts template 40)))
  (- max-count min-count))

(define (min-max-count counts)
  (let ((min-k #f)
        (min-v (apply max (hash-table-values counts)))
        (max-k #f)
        (max-v 0))
    (hash-table-for-each
      (lambda (k v)
        (if (< v min-v) (begin (set! min-k k) (set! min-v v)))
        (if (> v max-v) (begin (set! max-k k) (set! max-v v))))
      counts)
    (values min-v max-v)))

(define (memoize proc)
  (let ((memory (make-hash-table (make-default-comparator))))
    (lambda args
      (hash-table-ref memory args
                      (lambda ()
                        (let ((res (apply proc args)))
                          (hash-table-set! memory args res)
                          res))))))

(define (counts template n)
  (let ((res (make-hash-table (make-default-comparator))))
    (for-each (lambda (k)
                (hash-table-update!/default res k (lambda (v) (+ v 1)) 0))
              template)
    (apply counts+!
           res
           (map (lambda (l r) (pairwise-counts l r n))
                template (cdr template)))))

(define pairwise-counts
  (memoize
    (lambda (l r n)
      (if (= n 0)
        (hash-table (make-default-comparator))
        (let ((inner (hash-table-ref rules (cons l r))))
          (counts+! (hash-table (make-default-comparator) inner 1)
                    (pairwise-counts l inner (- n 1))
                    (pairwise-counts inner r (- n 1))))))))

(define (counts+! ht0 . hts)
  (for-each
    (lambda (ht)
      (hash-table-for-each
        (lambda (k v)
          (hash-table-update!/default ht0 k (lambda (old) (+ v old)) 0))
        ht))
    hts)
  ht0)


(define-values (template rules) (read-input (cadr (command-line))))
(write (part1 template rules)) (newline)
