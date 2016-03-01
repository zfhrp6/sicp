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

(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))

(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))

(define (equal-rat x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

(define (make-rat n d) (cons n d))

(define (numer x) (car x))

(define (denom x) (cdr x))

(define (print-rat x)
  (disp (numer x) "/" (denom x)))

(define one-half (make-rat 1 2))
(define one-third (make-rat 1 3))
(print-rat one-half)
(print-rat (add-rat one-half one-third))
(print-rat (mul-rat one-half one-third))
(print-rat (add-rat one-third one-third))
(print-rat (div-rat one-third one-half))

(define (make-rat n d)
  (let ((g (gcd n d)))
    (cons (/ n g) (/ d g))))
(print-rat one-half)
(print-rat (add-rat one-half one-third))
(print-rat (mul-rat one-half one-third))
(print-rat (add-rat one-third one-third))
(print-rat (div-rat one-third one-half))

; 問題2.1
(disp "*--- prob2.1")
(define (make-rat n d)
  (let ((g (gcd n d)))
    (if (< (* n d) 0)
        (cons (- (/ (abs n) g)) (/ (abs d) g))
        (cons (/ (abs n) g) (/ (abs d) g)))))

(define mtq (make-rat 3 -4))
(print-rat mtq)
(print-rat (add-rat mtq one-half))
(print-rat (mul-rat mtq mtq))

; 問題2.2
(disp "*--- prob2.2")
(define (print-point p)
  (disp "(" (x-point p) "," (y-point p) ")"))
(define (make-segment sp ep)
  (cons sp ep))
(define (start-segment seg)
  (car seg))
(define (end-segment seg)
  (cdr seg))
(define (make-point x y)
  (cons x y))
(define (x-point p)
  (car p))
(define (y-point p)
  (cdr p))
(define (midpoint-segment seg)
  (make-point (/ (+ (x-point (start-segment seg)) (x-point (end-segment seg))) 2)
              (/ (+ (y-point (start-segment seg)) (y-point (end-segment seg))) 2)))
(define origin (make-point 0 0))
(define tttt (make-point 33 33))
(define mipl (make-point 99 -0.4))
(define seg1 (make-segment tttt mipl))
(define seg2 (make-segment origin tttt))
(print-point (midpoint-segment seg2))
(print-point (midpoint-segment seg1))

; 問題2.3
(disp "*--- prob2.3")
(define (print-rect rect)
  (print-point (car rect))
  (print-point (cdr rect)))
(define (make-rectangle p1 p2)
  (cons p1 p2))
(define (height rect)
  (abs (- (y-point (car rect)) (y-point (cdr rect)))))
(define (width rect)
  (abs (- (x-point (car rect)) (x-point (cdr rect)))))
(define (area rect)
  (* (height rect) (width rect)))
(define (perimeter rect)
  (* 2 (+ (height rect) (width rect))))

(define rect1 (make-rectangle origin tttt))
(define rect2 (make-rectangle (make-point 1 1) origin))
(print-rect rect1)
(print-rect rect2)
(disp "area" (area rect1) (area rect2))

(define (cons2 x y)
  (define (dispatch m)
    (cond ((= m 0) x)
          ((= m 1) y)
          (else (error "Argument not 0 or 1 --CONS" m))))
  dispatch)
(define (car2 z) (z 0))
(define (cdr2 z) (z 1))

(define (cons3 x y)
  (lambda (m) (m x y)))
(define (car3 z)
  (z (lambda (p q) p)))
(define (cdr3 z)
  (z (lambda (p q) q)))
(disp (cdr3 (cons3 1 2)))

; 問題2.5
(disp "*--- prob2.5")
(define (cons4 a b) (* (expt 2 a) (expt 3 b)))
(define (car4 pr) (div_count pr 2))
(define (cdr4 pr) (div_count pr 3))
(define (div_count n a)
  (define (div-iter n a cnt)
    (cond ((= n 0) cnt)
          ((= 0 (remainder n a)) (div-iter (/ n a) a (+ 1 cnt)))
          (else cnt)))
  (div-iter n a 0))
(define abpr1 (cons4 3 4))
(define abpr2 (cons4 1231 23491))
(disp (car4 abpr2) (cdr4 abpr2))

; 問題2.6
(disp"*--- prob2.6")
(define zero (lambda (f) (lambda (x) x)))
(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))
(disp (add-1 zero))
(define one (lambda (f) (lambda (x) (f x))))
(define two (lambda (f) (lambda (x) (f (f x)))))
(define plus 8)
(define inc (lambda (x) (+ 1 x)))
(disp ((zero inc) 0))
(define (add-church m n)
  (lambda (f) (lambda (x) ((m f) ((n f) x)))))
(disp (((add-church one two) inc) 1))

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))
(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))
(define (div-interval x y)
  (mul-interval x
                (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y)))))

; 問題2.7
(define (make-interval a b) (cons a b))
(define (upper-bound iv)
  (max (car iv) (cdr iv)))
(define (lower-bound iv)
  (min (car iv) (cdr iv)))
(disp (add-interval (make-interval 100 1) (make-interval 99 3)))

; 問題2.8
(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))

; 問題2.8
(define (iv-width iv)
  (/ (- (upper-bound iv) (lower-bound iv)) 2.0))
(define iv1 (make-interval 2.8 5.8))
(define iv2 (make-interval 1.2 7.7))
(disp (iv-width iv1) (iv-width iv2))
(disp (sub-interval iv1 iv2) (iv-width (sub-interval iv1 iv2)))
(disp iv1 (iv-width iv1) iv2 (iv-width iv2) (iv-width (mul-interval iv1 iv2)) (iv-width (div-interval iv1 iv2)))

; 問題2.10
(disp (div-interval (make-interval 1.1 3.3) (make-interval -1.2 8.8)))
(define (div-interval2 x y)
  (if (< (* (lower-bound y) (upper-bound y)) 0) (error "denominator range contains 0" y)
      (mul-interval x
                    (make-interval (/ 1.0 (upper-bound y))
                                   (/ 1.0 (lower-bound y))))))
;(disp (div-interval (make-interval 1.1 3.3) (make-interval -1.2 8.8)))

; 問題2.11
; Benの言ってること
; (+, +), (+, +) -> (l*l, r*r)
; (-, +), (+, +) -> (l*r, r*r)
; (-, -), (+, +) -> (l*r, r*l)
; (+, +), (-, +) -> (r*l, r*r)
; (-, +), (-, +) -> (l*r or r*l, r*r) これだけ3回必要
; (-, -), (-, +) -> (r*r, l*l)
; (+, +), (-, -) -> (r*l, l*r)
; (-, +), (-, -) -> (r*l, l*l)
; (-, -), (-, -) -> (r*r, l*l)
; あとでやる
(disp "prob2.11" "---- あとでやる ----")

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))
(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))
(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

; 問題2.12
(define (make-center-percent c p)
  (make-interval (* c (- 1.00 (* 0.01 p))) (* c (+ 1.00 (* 0.01 p)))))
(define (percent i)
  (* (/ (- (upper-bound i) (center i)) (center i)) 100))
(define i1 (make-center-percent 82 8))
(disp (percent i1) (center i1))

; 問題2.13
; 十分小さい誤差 p1 p2 を持つ i1 iv1 に対して
; (c1+c1*p1)*(c2+c2*p2) ~> (c1*c2 + c1*c2*(p1+p2))
; なので (p1+p2) が積の誤差
(disp i1 (center i1) (percent i1))
(disp iv1 (center iv1) (percent iv1))
(disp (mul-interval i1 iv1) (center (mul-interval i1 iv1)) (percent (mul-interval i1 iv1)))

; (define (paraR1 r1 r2)
;   (/ (* 1.0 r1 r2) (+ r1 r2)))
; (define (paraR2 r1 r2)
;   (/ 1.0 (+ (/ 1.0 r1) (/ 1.0 r2))))
; (disp (paraR1 34 54) (paraR2 34 54))
(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))
(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval one
                  (add-interval (div-interval one r1)
                                (div-interval one r2)))))
(define r1 (make-center-percent 12 2))
(define r2 (make-center-percent 34 1))
(disp (par1 r1 r2) (par2 r1 r2))
(define i1 (make-center-percent 11111 1))
(define i2 (make-center-percent 3421 10))
(disp (div-interval i1 i1) (div-interval i1 i2) (div-interval i2 i2))

; 問題2.14
; 式変形中に R1/R1 や R2/R2 が存在して，これらが 1 じゃないので危ない

; 問題2.15
; 

; 問題2.16
; 一般的には出来ないがそれぞれのケースで出来たり出来なかったり (参考: https://en.wikipedia.org/wiki/Interval_arithmetic#Dependency_problem )


(cons 1 2)
(cons (cons 1 2) (cons 3 4))
(cons (cons 1 (cons 2 3)) 4)
(cons 1
      (cons 2
            (cons 3
                  (cons 4 '()))))
(list 1 2 3 4)
(define one-through-four (list 1 2 3 4))
(disp
(car one-through-four)
(cdr one-through-four)
(car (cdr one-through-four))
(cons 10 one-through-four)
(cons 5 one-through-four)
)

(define (list-ref2 items n)
  (if (= n 0)
    (car items)
    (list-ref (cdr items) (- n 1))))
(define squares (list 1 4 9 16 25))
(define (length2 items)
  (if (null? items)
      0
      (+ 1 (length (cdr items)))))
(define odds (list 1 3 5 7))
(define (length3 items)
  (define (length-iter a count)
    (if (null? a)
        count
        (length-iter (cdr a) (+ 1 count))))
  (length-iter items 0))
(define (append2 list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))

; 問題2.17
(define (last-pair list1)
  (if (= 1 (length list1))
      (car list1)
      (last-pair (cdr list1))))
(disp (last-pair (list 23 72 149 34)))

; 問題2.18
(define (reverse list1)
  (if (null? list1)
      list1
      (append (reverse (cdr list1)) (list (car list1)))))
(disp (reverse (list 1 4 9 16 25)))

; 問題2.19

