; Guile runtime
(define (runtime) (tms:clock (times)))

(define (list-length lst)
  (define (l-iter rest count)
    (if (null? rest)
        count
        (l-iter (cdr rest) (+ 1 count))))
  (l-iter lst 0))
(define pi (* 4 (atan 1)))
(define (square x) (* x x))

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (runtime) start-time))))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(define (sum term a next b)
  (if (> a b)
    0
    (+ (term a)
       (sum term (next a) next b))))

(define (sum_ term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ (term a) result))))
  (iter a 0))

(define (prod term a next b)
  (if (> a b)
    1
    (* (term a)
       (prod term (next a) next b))))

(define (prod_ term a next b)
  (define (iter a result)
    (if (> a b)
      result
      (iter (next a) (* (term a) result))))
  (iter a 1))

(define (inc n) (+ n 1))

(define (cube n) (* n n n))

(define (sum-cubes a b)
  (sum cube a inc b ))

(display (sum-cubes 1 10))(sum-cubes 1 10)
(newline)

(define (identity x) x)
(define (sum-integers a b)
  (sum identity a inc b))
(display (sum-integers 1 10))
(newline)

(define (pi-sum a b)
  (define (pi-term x)
         (/ 1.0 (* x (+ x 2))))
  (define (pi-next x)
    (+ x 4))
  (sum pi-term a pi-next b))
(display (* 8 (pi-sum 1 1000)))
(newline)

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b) dx))
(display (integral cube 0 1 0.001))
(newline)

(define (simpson f a b n)
  (define (add-h x) (+ x (/ (- b a) n)))
  (* (/ (- b a) n) (sum f )))

(define (bin-search f neg-point pos-point)
  (let ((midpoint (average neg-point pos-point)))
    (if (close-enough? neg-point pos-point)
      midpoint
      (let ((test-value (f midpoint)))
        (cond ((positive? test-value)
               (bin-search f neg-point midpoint))
              ((negative? test-value)
               (bin-search f midpoint pos-point))
              (else midpoint))))))

(define (close-enough? x y)
  (< (abs (- x y)) 0.001))

(define (half-interval-method f a b)
  (let ((a-value (f a))
        (b-value (f b)))
    (cond ((and (negative? a-value) (positive? b-value))
           (bin-search f a b))
          ((and (negative? b-value) (positive? a-value))
           (bin-search f b a))
          (else
            (error "Values are not of opposite sign" a b)))))

(define (average a b) (/ (+ a b) 2.0))

(display (half-interval-method sin 2.0 4.0))
(newline)

(define tolerance 0.000001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(display (fixed-point cos 1.0))
(newline)

(define (sqrt x)
  (fixed-point (lambda (y) (average y (/ x y)))
               1.0))
(display (sqrt 5))
(newline)

; 問題1.35
(display
  (fixed-point (lambda (y) (/ (+ y 1 (/ 1 y)) 2.0)) 1.0))
(newline)

; 問題1.36
; 平均緩和なし
(define (f1 x) (/ (log 1000) (log x)))
; 平均緩和あり
(define (f2 x) (/ (+ x (/ (log 1000) (log x))) 2.0))

(define (fixed-point_ f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (display "-> ") (display next)
      (newline)
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

; (fixed-point_ f2 950.1)

; 問題1.37
(define (cont-frac-i n d k)
  (define (frac-iter i result)
    ; (display i)
    (if (= i 0)
        result
        (frac-iter (- i 1) (/ (n i) (+ result (d i))))))
  (frac-iter (- k 1) (/ (n k) (d k))))
(display (cont-frac-i (lambda (i) 1.0)
                      (lambda (i) 1.0)
                      10))
(newline)

(define (cont-frac n d k)
  (define (frac i)
    (if (< i k)
        (/ (n i) (+ (d i) (frac (+ i 1))))
        (/ (n i) (d i))))
  (frac 1))
(display (cont-frac (lambda (i) 1.0)
                    (lambda (i) 1.0)
                    10))
(newline)

; 問題1.38
(define (e i)
  (if (= (remainder i 3) 2)
      (* 2 (+ 1 (quotient i 3)))
      1))
(display (+ 2 (cont-frac (lambda (x) 1.0)
                    e
                    17)))
(newline)

; 問題1.39
(define (od i) (- (* 2 i) 1))
(define (tan-cf x k)
  (define (mol i)
    (if (= i 1) x (- (* x x))))
  ; (define (frac-iter i result)
  ;   (if (= i 0)
  ;     result
  ;     (frac-iter (- i 1) (/ (mol i) (+ (od i) result)))))
  ; (frac-iter (- k 1) (/ (mol k) (od k))))
  (cont-frac mol od k))
(display (tan-cf (/ pi 4) 8))
(newline)

(define (average-damp f)
  (lambda (x) (average x (f x))))

((average-damp square) 10)

(define (sqrt_ x)
  (fixed-point (average-damp (lambda (y) (/ x y)))
               1.0))
(define (cube-root x)
  (fixed-point (average-damp (lambda (y) (/ x (square y))))
               1.0))

; display function
(display 3) (newline)
(display (sqrt_ 7)) (newline)
(define (disp x) (display x) (newline))
(disp 7) (disp (cube-root 7))
(define (disp . args)
  (define (disp-i i)
    (display (list-ref args i)) (display " ")
    (if (< i (- (list-length args) 1))
         (disp-i (+ i 1))
         (newline)))
  (disp-i 0))
  ; (if (= (list-length args) 0)
  ;     #f
  ;     (disp (cdr args))))
(disp "args" "display" "function" 4 5 6 "tetete")
(define (disp_nnl . args)
  (define (disp-i i)
    (display (list-ref args i)) (display " ")
    (if (< i (- (list-length args) 1))
        (disp-i (+ i 1))
        (display " ")))
  (disp-i 0))

(define dx 0.0001)

(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
       dx)))

(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) ((deriv g) x)))))

(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

(define (sqrt3 x)
  (newtons-method (lambda (y) (- (square y) x))
                  1.0))
(disp (sqrt3 31))

(define (fixed-point-of-transform g transform guess)
  (fixed-point (transform g) guess))

(define (sqrt4 x)
  (fixed-point-of-transform (lambda (y) (/ x y))
                            average-damp
                            1.0))

(define (sqrt5 x)
  (fixed-point-of-transform (lambda (y) (- (square y) x))
                            newton-transform
                            1.0))

; 問題 1.41
(define (cubic a b c)
  (lambda (x) (+ (* x x x) (* a x x) (* b x) c)))

(disp (newtons-method (cubic -3 3 -1) 100))

; 問題1.42
(disp_nnl "prob1.42")
(define (compose f g)
  (lambda (y) (f (g y))))

(disp ((compose square inc) 6))

; 問題1.43
(disp_nnl "prob1.43")
(define (repeated f n)
  (if (= n 1)
      f
      (compose f (repeated f (- n 1)))))

(disp ((repeated square 3) 5))

; 問題1.44
(define (smooth f)
  (let ((dx 0.0001))
       (lambda (x) (/ (+ (f x) (f (+ x dx)) (f (- x dx))) 3.0))))
(define (fs x) (+ 3 (* x x)))
(disp_nnl "prob1.44")
(disp ((smooth fs) 4))
(define (nth-smooth f n)
  ((repeated smooth n) f))
(disp ((nth-smooth fs 6) 4))

(define (cube-root x)
  (fixed-point (average-damp (lambda (y) (/ x (square y))))
               1.0))
; 問題1.45
(define (nth-root num n k)
  (fixed-point ((repeated average-damp k) (lambda (y) (/ num (expt num (- n 1))))) 1.0))
(disp (nth-root 5 3 2))
(define (nth-root x n)
  (fixed-point ((repeated average-damp (floor (/ (log n) (log 2))))
                 (lambda (y) (/ x (expt y (- n 1)))))
               1.0))
(disp "prob1.45" (nth-root 3800900000000000 17))

; 問題1.46
(disp_nnl "prob1.46")
(define (iterative-improve check improve)
  (define (iter guess)
    (if (check guess)
        guess
        (iter (improve guess))))
  iter)
  ; (lambda (guess)
  ;         (if (check guess)
  ;             guess
  ;             (iterative-improve check (improve guess)))))
(define (ss x) (< x 1.1))
(define (ii x) (- x 0.01))
(disp ((iterative-improve ss ii) 3.0))

(define (sqrt6 x)
  (define (good-enough? guess)
    (< (abs (- (square guess) x)) 0.000000001))
  (define (improv guess)
    (average guess (/ x guess)))
  ((iterative-improve good-enough? improv) 1.0))
 (disp (sqrt6 6) (sqrt5 6) (sqrt4 6) (sqrt3 6))

(define (fixed-point2 f guess)
  ((iterative-improve (lambda (y)
                        (< (abs (- (f y) y)) 0.00001))
                      (lambda (y)
                        (f y)))
   guess))
(define (fixed-point3 f guess)
  ((iterative-improve (lambda (y)
                        (< (abs (- (f y) y)) 0.00001))
                      (lambda (y)
                        (/ (+ y (f y)))))
   guess))
(disp (fixed-point2 (lambda (x) (+ 1 (/ 1 x))) 2.0))
(disp (fixed-point2 (lambda (x) (+ (- x) 10)) 1.01))
