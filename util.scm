(define-module sicputil
  (export disp
          list-length))

(select-module sicputil)

(define (disp . args)
  (define (disp-i i)
    (display (list-ref args i)) (display " ")
    (if (< i (- (list-length args) 1))
         (disp-i (+ i 1))
         (newline)))
  (disp-i 0))

(define (list-length lst)
  (define (l-iter rest count)
    (if (null? rest)
        count
        (l-iter (cdr rest) (+ 1 count))))
  (l-iter lst 0))

(define (mapf func . seq)
  (define (func-i i)
    (func (list-ref seq i))
    (if (< i (- (list-length seq) 1))
        (func-i (+ i 1))
        ))
  (func-i 0))

(provide "sicputil")

