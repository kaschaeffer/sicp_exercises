; CHAPTER 2

; Note: SICP assumes nil is defined
; but the current version of MIT Scheme
; does not include it
;
; to fix this, we define it as '()
(define nil '())

; working with rational numbers

; Exercise 2.1
; improving the rational number
; constructor function, make-rat
(define (make-rat n d)
  (let ((g (gcd n d)))
    (if (negative? d)
        (cons (* -1 (/ n g)) (* -1 (/ d g)))
        (cons (/ n g) (/ d g)))))
(define (numer x) (car x))
(define (denom x) (cdr x))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

; Exercise 2.2
; Working with line segments in the plane

(define (average a b) (/ (+ a b) 2))

(define (make-point x y)
  (cons x y))
(define (x-point p) (car p))
(define (y-point p) (cdr p))

(define (make-segment p1 p2)
  (cons p1 p2))
(define (start-segment seg) (car seg))
(define (end-segment seg) (cdr seg))

(define (midpoint-segment seg)
  (let ((start (start-segment seg))
        (end (end-segment seg)))
    (make-point (average (x-point start) (x-point end))
                (average (y-point start) (y-point end)))))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

; EXERCISE 2.3
; TO DO!!!!!!!!!!!!!!!!!

; A silly but instructive implementation of
; cons, car, cdr...
;
; apparently this is also an example of the
; "message passing" paradigm...
;
(define (cons2 x y)
  (define (dispatch m)
    (cond ((= m 0) x)
          ((= m 1) y)
          (else (error "Argument not 0 or 1: CONS"))))
  dispatch)

(define (car2 z) (z 0))
(define (cdr2 z) (z 1))

; Exercise 2.4
; Yet another way of implementing pairs
(define (cons3 x y)
  (lambda (m) (m x y)))

(define (car3 z)
  (z (lambda (p q) p)))

(define (cdr3 z)
  (z (lambda (p q) q)))
    
; EXERCISE 2.5
; TO DO

; Exercise 2.6
; Church Numerals
;
; -- a way of encoding the natural numbers
; via higher order functions
;
; the number 'n' is encoded by a higher 
; order function that maps f --> f^n
; (where f^n means compose f with itself
; n times)
;
; this is totally crazy!!
;
(define zero (lambda (f) (lambda (x) x)))
(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

(define one 
  (lambda (f) (lambda (x) (f x))))

(define two
  (lambda (f) (lambda (x) (f (f x)))))

(define (add a b)
  (lambda (f) (lambda (x) ((b f) ((a f) x)))))

(define (inc n)
  (+ n 1))

; Exercise 2.7
; an interval constructor
(define (make-interval a b) (cons a b))
(define (upper-bound int) (cdr int))
(define (lower-bound int) (car int))

; Exercise 2.8
; interval subtraction
(define (sub-interval x y)
  (make-interval (- (upper-bound x) (lower-bound y))
                 (- (lower-bound x) (upper-bound y))))

; Exercise 2.9
; How the width of intervals is modified by operations
;
; for addition and subtraction, w_1 + w_2 = w_result
;
; However, this is not true for multiplication
; for example:
; take interval_1 = [-1, 1]
; and  interval_2 = [100, 102]
; the result of multiplying is:
; interval_result = [-102, 102]
;
; but width_1 * width_2 = 1
; while width_result = 102

; TO DO Exercises 2.10 - 2.16
; !!!!!!!!!!!!!

; TO DO Exercises 2.17-2.20

(define (scale-list items factor)
  (if (null? items)
      nil
      (cons (* (car items) factor)
            (scale-list (cdr items) factor))))

; an implementation of map
(define (map2 proc items)
  (if (null? items)
      nil
      (cons (proc (car items))
            (map proc (cdr items)))))

; Exercise 2.21
(define (square-list items)
  (if (null? items)
      nil
      (cons (square (car items))
            (square-list (cdr items)))))

(define (square-list items)
  (map (lambda (x) (* x x)) items))

(define (square x)
  (* x x))

; Exercise 2.22
(define (square-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons answer
                    (square (car things))))))
  (iter items nil))

; Exercise 2.23
(for-each2 (lambda (x)
            (newline)
            (display x))
          (list 57 200 982))

(define (for-each2 f items)
  (f (car items))
  (if (> (length items) 1)
      (for-each2 f (cdr items))))
      
; Exercises 2.24 - 2.26

; Exercise 2.27
; Deep-reverse

(define (reverse items)
  (define (reverse-iter items result)
    (if (null? items)
        result
        (reverse-iter (cdr items) (cons (car items) result))))
  (reverse-iter items nil))


(define (deep-reverse items)
  (define (deep-reverse-iter items result)
    (if (null? items)
        result
        (if (pair? (car items))
            (deep-reverse-iter (cdr items) (cons (deep-reverse (car items)) result))
            (deep-reverse-iter (cdr items) (cons (car items) result)))))
  (deep-reverse-iter items nil))

; TO DO 2.28 2.29

; Exercise 2.30
(define (square-tree tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (square-tree sub-tree)
             (* sub-tree sub-tree))) tree))

; Exercise 2.31
(define (tree-map f tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (tree-map f sub-tree)
             (f sub-tree))) tree))

(define (square x)
  (* x x))

(define (square-tree2 tree)
  (tree-map square tree))

; Exercise 2.53
; Simple examples using memq
(define (memq item x)
  (cond ((null? x) false)
        ((eq? item (car x)) x)
        (else (memq item (cdr x)))))

; Exercise 2.54
; to do...

; Exercise 2.55
; ''abracadabra is expanded out as
; '(quote abracadabra)
; so that (car ''abracadabra)
; equals quote

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        ((sum? exp) (make-sum (deriv (addend exp) var)
                              (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
           (make-product (multiplier exp)
                         (deriv (multiplicand exp) var))
           (make-product (deriv (multiplier exp) var)
                         (multiplicand exp))))
        ((exponentiation? exp)
         (make-product (make-product (exponent exp) 
                       (make-exponentiation (base exp) (make-sum (exponent exp) -1)))
                       (deriv (base exp) var)))                                            
        (else
          (error "unknown expression type: DERIV" exp))))

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2))
         (+ a1 a2))
        (else (list '+ a1 a2))))

(define (make-product a1 a2)
  (cond ((=number? a1 1) a2)
        ((=number? a2 1) a1)
        ((=number? a1 0) 0)
        ((=number? a2 0) 0)
        ((and (number? a1) (number? a2))
         (* a1 a2))
        (else (list '* a1 a2))))

(define (sum? x) (and (pair? x) (eq? (car x) '+)))

(define (addend s) (cadr s))

(define (augend s) (caddr s))

(define (product? x) (and (pair? x) (eq? (car x) '*)))

(define (multiplier p) (cadr p))

(define (multiplicand p) (caddr p))

(define (=number? exp num) (and (number? exp) (eq? exp num)))

; Exercise 2.56
; Extending differentiation to work for x^n

(define (exponentiation? exp) (and (pair? exp) (eq? (car exp) '**)))

(define (make-exponentiation base exponent) 
  (cond ((and (number? exponent) (eq? exponent 0)) 1)
        ((and (number? exponent) (eq? exponent 1)) base)
        (else (list '** base exponent))))

(define (base exp) (cadr exp))

(define (exponent exp) (caddr exp))

; Exercise 2.57
; Dealing with multiplying and adding more than two numbers

(define (addend s) (cadr s))

(define (augend s) 
  (cond ((> (length s) 3) (cons '+ (cddr s)))
        (else (caddr s)))))

(define (multiplier p) (cadr p))

(define (multiplicand p)
  (cond ((> (length p) 3) (cons '* (cddr p)))
        (else (caddr p))))

; Exercise 2.58(a)
; Doing everything with **infix** notation

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2))
         (+ a1 a2))
        (else (list a1 '+ a2))))

(define (make-product a1 a2)
  (cond ((=number? a1 1) a2)
        ((=number? a2 1) a1)
        ((=number? a1 0) 0)
        ((=number? a2 0) 0)
        ((and (number? a1) (number? a2))
         (* a1 a2))
        (else (list a1 '* a2))))

(define (sum? x) (and (pair? x) (eq? (cadr x) '+)))

(define (addend s) (car s))

(define (augend s) (caddr s))

(define (product? x) (and (pair? x) (eq? (cadr x) '*)))

(define (multiplier p) (car p))

(define (multiplicand p) (caddr p))

; Exercise 2.58(b)
; Working with **infix** notation but deal appropriately
; with cases where the expression is not fully parenthesized


(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2))
         (+ a1 a2))
        (else (list a1 '+ a2))))

(define (make-product a1 a2)
  (cond ((=number? a1 1) a2)
        ((=number? a2 1) a1)
        ((=number? a1 0) 0)
        ((=number? a2 0) 0)
        ((and (number? a1) (number? a2))
         (* a1 a2))
        (else (list a1 '* a2))))

(define (sum? x) (and (pair? x) (eq? (cadr x) '+)))

(define (addend s) (car s))

(define (augend s) 
  (cond ((> (length s) 3) (cddr s))
        (else (caddr s)))))

(define (product? x) (and (pair? x) (eq? (cadr x) '*)))

(define (multiplier p) (car p))

(define (multiplicand p) (caddr p))

; Representing sets
; First approach: unordered list

; element-of-set is O(n)
(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

; insertion O(1)
(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))

; intersection O(n1 * n2)
(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
         (cons (car set1)
               (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))

; Exercise 2.59
; Implement union

(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        ((element-of-set? (car set1) set2)
         (union-set (cdr set1) set2))
        (else (cons (car set1) (union-set (cdr set1) set2)))))

; Exercise 2.60
; What if the original lists already have duplicates in them?
; how do we deal with this?


; element-of-set, insertion, and intersection are all
; implemented in the same way as before

; However, union can be much faster (it does 
; not use element-of-set? at all)
(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        (else (cons (car set1) (union-set (cdr set1) set2)))))

; Implementing sets
; Second approach: a sorted list

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (car set)) true)
        ((< x (car set)) false)
        (else (element-of-set? x (cdr set)))))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((= (car set1) (car set2)) (cons (car set1) 
                                         (intersection-set (cdr set1) (cdr set2))))
        ((< (car set1) (car set2)) (intersection-set (cdr set1) set2))
        (else (intersection-set set1 (cdr set2)))))

; Exercise 2.61
(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((<= x (car set)) (cons x set))
        (else (cons (car set) (adjoin-set x (cdr set))))))

; Exercise 2.62
(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        ((= (car set1) (car set2)) (cons (car set1) (union-set (cdr set1) (cdr set2))))
        ((< (car set1) (car set2)) (cons (car set1) (union-set (cdr set1) set2)))
        ((> (car set1) (car set2)) (cons (car set2) (union-set set1 (cdr set2))))))

; Implementing sets
; Third approach: a binary tree

(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (entry set)) true)
        ((< x (entry set)) (element-of-set? x (left-branch set)))
        ((> x (entry set)) (element-of-set? x (right-branch set)))))

(define (adjoin-set x set)
  (cond ((null? set) (make-tree x '() '()))
        ((= x (entry set)) set)
        ((< x (entry set)) (make-tree (entry set)
                                      (adjoin-set x (left-branch set))
                                      (right-branch set)))
        ((> x (entry set)) (make-tree (entry set)
                                      (left-branch set)
                                      (adjoin-set x (right-branch set))))))

; TO DO: Exercises 2.63 - 2.66

; Stopped at p. 209

; Huffman encoding
(define (make-leaf symbol weight)
  (list 'leaf symbol weight))

(define (leaf? object)
  (eq? (car object) 'leaf))

(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left)
                (symbols right))
        (+ (weight left)
           (weight right))))

(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))

(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

; Huffman Tree decoding
(define (decode bits tree)
  
  (define (choose-branch bit tree)
    (cond ((eq? bit 0) (left-branch tree))
          ((eq? bit 1) (right-branch tree))))
  
  (define (decode-one bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch (choose-branch (car bits) current-branch)))
         (if (leaf? next-branch)
             (cons (symbol-leaf next-branch) (decode-one (cdr bits) tree))
             (decode-one (cdr bits) next-branch)))))
  
  (decode-one bits tree))
  
(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set)))
         (cons x set))
        (else (cons (car set) (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair)
                               (cadr pair))
                    (make-leaf-set (cdr pairs))))))
  
; Exercise 2.67
; Sample Huffman Decoding
(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                    (make-leaf 'B 2)
                    (make-code-tree (make-leaf 'D 1)
                                    (make-leaf 'C 1)))))

(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

(decode sample-message sample-tree)

; Exercise 2.68
; Huffman Encoding procedure

(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

(define (encode-symbol symbol tree)
  
  (define (encode-symbol-recur symbol branch code)
    (if (leaf? branch)
        (if (eq? (symbol-leaf branch) symbol)
            code
            (error "bad leaf"))
        (cond ((contains? (symbols (left-branch branch)) symbol)
               (encode-symbol-recur symbol (left-branch branch) (append code '(0))))
              ((contains? (symbols (right-branch branch)) symbol)
               (encode-symbol-recur symbol (right-branch branch) (append code '(1))))
              (else (error "symbol not in tree")))))
  
  (encode-symbol-recur symbol tree '()))


(define (contains? set element)
  (if (null? set)
      false
      (if (eq? element (car set))
          true
          (contains? (cdr set) element))))

       
;Test it
(encode (list 'A 'D 'A 'B 'B 'C 'A) sample-tree)

; Exercise 2.69

;
  
      
    

