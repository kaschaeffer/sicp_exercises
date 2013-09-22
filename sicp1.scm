(define (abs x)
  (if (< x 0)
      (- x)
      x))

; Exercise 1.1
; Practice with prefix operations
(= (+ 5 3 4) 12)
(= (- 9 1) 8)
(= (/ 6 2) 3)
(= (+ (* 2 4) (- 4 6)) 6)
(define a 3)
(define b (+ a 1))
(= (+ a b (* a b)) 19)

; Exercise 1.2
; Practice translating infix to prefix
(/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 3))))) (* 3 (- 6 2) (- 2 7)))

(define (two-sum-of-squares a b c)
  (cond ((and (>= a c) (>= b c)) (+ (square a) (square b)))
        ((and (>= a b) (>= c b)) (+ (square a) (square c)))
        ((and (>= b a) (>= c a)) (+ (square b) (square c)))))
  

(define (square x)
  (* x x))

; Exercise 1.5
; Applicative-order vs. Normal-order evalutation

(define (p) (p))

(define (test x y)
  (if (= x 0)
      0
      y))

; Now if you call (test 0 (p)) this hangs if the interpreter
; is using applicative evaluation (this is true for scheme)
; However, if we were using normal-order evaluation, this 
; would simply return 0


; Exercise 1.7
; An improved good-enough? function

; First it is instructive to better understand
; the problem with the existing sqr-root method
; 
; For small values this is clue.  Why is this
; an issue for larger values?

(define (sqrt x)
  (sqrt-iter 1.0 0.0 x))

(define (sqrt-iter guess last-guess x)
  (if (good-enough? guess last-guess)
      guess
      (sqrt-iter (improve guess x) 
                 guess
                 x)))

(define (improve guess x)
  (/ (+ guess (/ x guess)) 2))

(define (good-enough? guess last-guess)
  (< (abs (- guess last-guess)) (* 0.001 guess)))


; Exercise 1.8
; Newton's method for cube-roots

(define (cube-root x)
  (cube-root-iter 1.0 x))

(define (cube-root-iter guess x)
  (if (good-enough? guess x)
      guess
      (cube-root-iter (improve guess x) x)))

(define (improve guess x)
  (/ (+ (/ x (square guess)) (* 2 guess)) 3))

(define (good-enough? guess x)
  (< (abs (- (cube guess) x)) 0.001))

(define (cube x)
  (* x x x))

; Experimenting with lexical and static scoping
;
(define a 17)
(define (my-print-a)
  (display a)
  (newline))
(define a 1717)
(let ((a 8))
  (my-print-a))

(define (my-func1)
  (my-func2))
(define (my-func2)
  (let ((b 2))
    (my-func3)))
(define (my-func3)
  (display b))
(let ((b 1))
  (my-func1))

(define (factorial n)
  (fact-iter 1 1 n))

(define (fact-iter product counter max-count)
  (if (> counter max-count)
      product
      (fact-iter (* counter product)
                 (+ counter 1)
                 max-count)))

; Exercise 1.9
;

; Recursive implementation of +
; using inc, dec
;
; (define (+ a b)
;   (if (= a 0)
;       b
;       (inc (+ (dec a) b))))


; Iterative implementation of +
; using inc, dec
; (Note: this will automatically be
; optimized by tail-call recursion)
;
; (define (+ a b)
;   (if (= a 0)
;       b
;       (+ (dec a) (inc b))))

; Exercise 1.10
;
; Ackermann's Function

(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))))))

; Exercise 1.11
; Recursive version
(define (f n)
  (if (< n 3)
      n
      (+ (+ (f (- n 1)) 
         (* (f (- n 2)) 2))
         (* (f (- n 3)) 3))))

; Faster iterative version
(define (g n)
  (f-iter 0 1 2 n))

(define (f-iter n1 n2 n3 n)
  (if (= n 1)
      n2
      (f-iter n2 n3 (+ (+ n3 (* n2 2)) (* n1 3)) (- n 1))))

; Exercise 1.12
; Pascal's Triangle

(define (pascals level i)
  (if (or (= i 0) (= i level))
      1
      (+ (pascals (- level 1) i) 
         (pascals (- level 1) (- i 1)))))



; Exercise 1.15
(define (cube x) (* x x x))
(define (p x) (- (* 3 x) (* 4 (cube x))))
(define (sine angle)
  (if (not (> (abs angle) 0.1))
      angle
      (p (sine (/ angle 3.0)))))

(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))

; Exercise 1.16
; Fast exponentiation using an
; iterative algorithm
;
; runs in O(log n) time

(define (fast-expt b n)
  (fast-expt-iter 1 b n))

(define (fast-expt-iter a b n)
  (cond ((= n 0) 1)
        ((= n 1) (* a b))
        ((even? n) (fast-expt-iter a (* b b) (/ n 2)))
        (else (fast-expt-iter (* a b) b (- n 1)))))

; Exercise 1.17
; Fast multiplication

(define (double a)
  (+ a a))

(define (halve a)
  (/ a 2))

; recursive version
(define (mult a b)
  (cond ((= b 0) 0)
        ((= b 1) a)
        ((even? b) (mult (double a) (halve b)))
        (else (+ a (mult a (- b 1))))))

; iterative version
(define (mult a b)
  (mult-iter 0 a b))

(define (mult-iter sum a b)
  (cond ((= b 0) 0)
        ((= b 1) (+ sum a))
        ((even? b) (mult-iter sum (double a) (halve b)))
        (else (mult-iter (+ sum a) a (- b 1)))))

; Exercise 1.19
; Computing Fibonacci numbers
; in O(log n) time!!

(define (fib n)
  (fib-iter 1 0 0 1 n))

(define (fib-iter a b p q count)
  (cond ((= count 0) b)
        ((even? count)
         (fib-iter a
                   b
                   (+ (* p p) (* q q))
                   (+ (* 2 p q) (* q q))
                   (/ count 2)))
        (else (fib-iter (+ (* b q) (* a q) (* a p))
                        (+ (* b p) (* a q))
                        p 
                        q
                        (- count 1)))))
         
; Euclid's algorithm
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

; Exercise 1.20
; Normal-order (lazy) evaluation of Euclid's algorithm
; vs.
; Applicative-order evaluation
;
; We want to know how many times 'remainder' is called
; in (gcd 206 40)
; 
; As explained in gcd, for this input we get
; (gcd 206 40)
; (gcd 40 6)
; (gcd 6 4)
; (gcd 4 2)
; (gcd 2 0)
;
; In the lazy evaluation method, we get nested remainder calls
; which are only executed to check (= b 0).  However, the evaluated
; number is *not* then passed on to the next call.  So,
; we have to call remainder 1 + 2 + 4 + 7 times when checking (= b 0)
; and then 4 more times to evaluate "a" at the end.
;
; In total, 18 times.
; 
; In contrast, using applicative evaluation we only call remainder
; 4 times (once per iteration).

; Exercise 1.21
; Finding smallest divisors
(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (square a)
  (* a a))

(smallest-divisor 199) ; = 199
(smallest-divisor 1999) ; = 1999
(smallest-divisor 19999) ; = 7

; Exercise 1.22
; Benchmarking a search for prime #s

(define (prime? n)
  (= n (smallest-divisor n)))

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (process-time-clock)))

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (process-time-clock) start-time))))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(define (search-for-primes n-min n-max)
  (define (search-for-primes-iter n n-max)
    (timed-prime-test n)
    (if (< n n-max)
        (search-for-primes-iter (+ n 1) n-max)))
  (search-for-primes-iter n-min n-max))

; Exercise 1.23
; A slightly more efficient method
; for finding the smallest-divisor
;
; this is about 1.6 times faster

(define (next a)
  (if (= a 2)
      3
      (+ a 2)))
         
(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor)))))

; can improve this even more...
; actually about twice as fast!

(define (smallest-divisor n)
  (if (= (remainder n 2) 0)
      2
      (find-divisor n 3)))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ 2 test-divisor)))))

; Exercise 1.25
; fast exponentiation then take mod
; versus fast modular exponentiation

(define (expmod base exp m)
  (cond ((= exp 1) (remainder base m))
        ((even? exp) (expmod (remainder (* base base) m) (/ exp 2) m))
        (else (remainder (* base (expmod base (- exp 1) m)) m))))

(define (expmod2 base exp m)
  (remainder (fast-expt base exp) m))

; notice that the second method is **much** slower since it doesn't
; reduce large numbers to their equivalent value (mod m)
; this leads to exponentiation of very large numbers, which slows
; down the process considerably...

; Exercise 1.26
; TO DO!!!!!!!!!

; Exercise 1.27
; Testing Carmichael numbers

(define (carmichael? n)
  )

; Exercise 1.28
; TO DO!!!!!!!!!!!!

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (cube x) (* x x x))
(define (inc n) (+ n 1))
(define (sum-cubes a b)
  (sum cube a inc b))

(define (identity x) x)
(define (sum-integers a b)
  (sum identity a inc b))

(define (pi-sum a b)
  (define (pi-term x)
    (/ 1.0 (* x (+ x 2))))
  (define (pi-next x)
    (+ x 4))
  (sum pi-term a pi-next b))


(define (pi-sum2 a b)
  (define (pi-term x)
    (/ 1.0 (* x (+ x 2))))
  (define (pi-next x)
    (+ x 4))
  (sum2 pi-term a pi-next b))

(define (integral f a b dx)
  (define (add-dx x)
    (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b) dx))

; Exercise 1.29
; An improved numerical integration function
; using Simpson's rule

(define (simpson-integral f a b n)
  (define (simpson-factor m)
    (cond ((or (= m 0) (= m n)) 1)
          ((even? m) 2)
          (else 4)))
  (define (fn m)
    (* (f (+ a (* (/ m n) (- b a))))
       (simpson-factor m) (/ 1 3)))
  (* (sum fn 0 inc n) (/ (- b a) n)))

; Exercise 1.30
; iterative version of the sum procedure

(define (sum2 term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ result (term a)))))
  (iter a 0))

; Exercise 1.31
; iterative product of terms
(define (product term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* result (term a)))))
  (iter a 1))

; recursive product of terms
(define (product term a next b)
  (if (> a b)
      1
      (* (term a) (product term (next a) next b))))

; Using this to compute PI/4
(define (pi n)
  (define (pi-term x)
    (cond ((= x 0) (/ 2 3))
          ((even? x) (/ (+ x 2) (+ x 3)))
          (else (/ (+ x 3) (+ x 2)))))
  (* 4.0 (product pi-term 0 inc n)))
  
; Exercise 1.32
; a general "accumulator" funciton

; recursive version
(define (accumulator combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a) 
                (accumulator combiner null-value term 
                             (next a) next b))))

; iterative version
(define (accumulator combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner result (term a)))))
  (iter a null-value))

; simple test
(define (pi-sum3 a b)
  (define (pi-term x)
    (/ 1.0 (* x (+ x 2))))
  (define (pi-next x)
    (+ x 4))
  (accumulator + 0 pi-term a pi-next b))

; Exercise 1.33
; A filtered accumulator function
; (i.e. functions not satisfying the filter function
; are not added to the sum)

(define (filtered-accumulator combiner 
                              null-value 
                              term 
                              a 
                              next 
                              b 
                              filter)
  (define (iter a result)
    (cond ((> a b) result)
          ((filter a)
           (iter (next a) (combiner result (term a))))
          (else
            (iter (next a) result))))
  (iter a null-value))

(define (prime-sum a b)
  (filtered-accumulator + 0 identity a inc b prime?))

(define (gcd-product n)
  (define (rel-prime? a)
    (= (gcd a n) 1))
  (filtered-accumulator * 1 identity 1 inc (- n 1) rel-prime?))

(define (pi-sum a b)
  (sum (lambda (x) (/ 1.0 (* x (+ x 2))))
       a
       (lambda (x) (+ x 4))
       b))

; Exercise 1.34
(define (f g) (g 2))

(f square)

(f (lambda (z) (* z (+ z 1))))

; Finding roots via binary search
(define (search f neg-point pos-point)
  (let ((midpoint (average neg-point pos-point)))
    (if (close-enough? neg-point pos-point)
        midpoint
        (let ((test-value (f midpoint)))
          (cond ((positive? test-value)
                 (search f neg-point midpoint))
                ((negative? test-value)
                 (search f midpoint pos-point))
                (else midpoint))))))

(define (close-enough? x y) (< (abs (- x y)) 0.001))

(define (average a b) (/ (+ a b) 2))

(define (half-interval-method f a b)
  (let ((a-value (f a))
        (b-value (f b)))
    (cond ((and (negative? a-value) (positive? b-value))
           (search f a b))
          ((and (negative? b-value) (positive? a-value))
           (search f b a))
          (else (error "Values are not of opposite sign" a b)))))

(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2))
       tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (sqrt x)
  (fixed-point (lambda (y) (/ (+ y (/ x y)) 2)) 1.0))

; Exercise 1.35
; Computing the Golden Ratio by finding a fixed-point
(fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0)

; TO DO 
; Exercises 1.36

; Exercise 1.37
; Continued fractions
; recursive version
(define (cont-frac n d k)
  (define (recur i)
    (if (> i k)
        0
        (/ (n i) (+ (d i) (recur (inc i))))))
  (recur 1))

; As an example, we can compute 1/(the golden ratio)
; for k = 10 this is actually accurate to 4 decimal
; places!

(cont-frac (lambda (i) 1.0) (lambda (i) 1.0) 10)

; Continued fractions
; iterative version
(define (cont-frac2 n d k)
  (define (iter i result)
    (if (< i 1)
        result
        (iter (- i 1) (/ (n i) (+ (d i) result)))))
  (iter k 0))

(cont-frac2 (lambda (i) 1.0) (lambda (i) 1.0) 10)

; Exercise 1.38
; Approximating e using Euler's continued fraction
(+ 2 (cont-frac2 (lambda (i) 1.0)
            (lambda (i)
              (cond ((= (remainder i 3) 0) 1)
                    ((= (remainder i 3) 1) 1)
                    ((= (remainder i 3) 2) (/ (* (+ i 1) 2) 3))))
            10))

; Exercise 1.39
; Approximating tangent using Lambert's formula
(define (tan-cf x k)
  (cont-frac (lambda (i)
               (if (= i 1)
                   x
                   (* x x -1.0))) 
             (lambda (i) (- (* 2 i) 1)) 
             k))

(define (average-damp f) (lambda (x) (average x (f x))))

(define (deriv g)
  (lambda (x) (/ (- (g (+ x dx)) (g x)) dx)))

(define dx 0.00001)

; Newton's method
(define (newton-transform g)
  (lambda (x) (- x (/ (g x) ((deriv g) x)))))
(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

; TO DO
; Exercises 1.40 - 1.46!!!!!
(list '2 '3)

