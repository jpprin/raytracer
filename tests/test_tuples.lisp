(in-package :com.godel.tuples)

(progn
  (load "p:/scripts/lisp/ray/rt.lisp")
  (load "p:/scripts/lisp/ray/tuples.lisp"))

(deftest tup-1
    (let ((a (make-point 4.3 -4.2 3.1)))
      (and
       (= (point-x a) 4.3)
       (= (point-y a) -4.2)
       (= (point-z a) 3.1)
       (= (point-w a) 1.0)
       (is-point a)
       (not (is-vec a)))) t)

(deftest tup-2
    (let ((p (make-point 4 -4 3)))
      (equalp p (make-tuple 4 -4 3 1))) t)

(deftest tup-3
  (let ((v (make-vec 4 -4 3)))
    (equalp v (make-tuple 4 -4 3 0))) t)

(deftest tup-4
  (let ((a 1.9)
      (b 1.8)
      (c 2.3)
      (d 2.3))
  (and
   (not (floats-equal-p a b))
   (floats-equal-p c d))) t)

(deftest tup-5
  (let ((a1 (make-tuple 3 -2 5 1))
      (a2 (make-tuple -2 3 1 0)))
  (equalp (add-tuples a1 a2)
	  (make-tuple 1 1 6 1))) t)

(deftest tup-6
  (let ((p1 (make-point 3 2 1))
      (p2 (make-point 5 6 7)))
  (equalp (subtract-tuples p1 p2)
	  (make-vec -2 -4 -6))) t)

(deftest tup-7
(let ((p (make-point 3 2 1))
      (v (make-vec 5 6 7)))
  (equalp (subtract-tuples p v) (make-point -2 -4 -6))) t)

(deftest tup-8
(let ((v1 (make-vec 3 2 1))
      (v2 (make-vec 5 6 7)))
  (equalp (subtract-tuples v1 v2) (make-vec -2 -4 -6))) t)

(deftest tup-9
(let ((zero-vec (make-vec 0 0 0))
      (v (make-vec 1 -2 3)))
  (equalp (subtract-tuples zero-vec v)
	  (make-vec -1 2 -3))) t)

(deftest tup-10
(let ((a (make-tuple 1 -2 3 -4)))
  (equalp (negate-tuple a)
	  (make-tuple -1 2 -3 4))) t)

(deftest tup-11
(let ((a (make-tuple 1 -2 3 -4)))
  (equalp (scalar-multiply a 3.5)
	  (make-tuple 3.5 -7 10.5 -14))) t)

(deftest tup-12
(let ((a (make-tuple 1 -2 3 -4)))
  (equalp (scalar-multiply a 0.5)
	  (make-tuple 0.5 -1 1.5 -2))) t)

(deftest tup-13
(let ((a (make-tuple 1 -2 3 -4)))
  (equalp (scalar-divide a 2)
	  (make-tuple 0.5 -1 1.5 -2))) t)

(deftest tup-14
(let ((v1 (make-vec 1 0 0))
      (v2 (make-vec 0 1 0))
      (v3 (make-vec 0 0 1))
      (v4 (make-vec 1 2 3))
      (v5 (make-vec -1 -2 -3)))
  (and
   (= 1 (magnitude v1))
   (= 1 (magnitude v2))
   (= 1 (magnitude v3))
   (= (sqrt 14) (magnitude v4))
   (= (sqrt 14) (magnitude v5)))) t)

(deftest tup-15
(let ((v1 (make-vec 4 0 0))
      (v2 (make-vec 1 2 3)))
  (and
   (equalp (normalize v1)
	   (make-vec 1 0 0))
   (equalp (normalize v2)
	   (make-vec (/ 1 (sqrt 14)) (/ 2 (sqrt 14)) (/ 3 (sqrt 14)))))) t)

(deftest tup-16
(let ((v (make-vec 1 2 3)))
  (floats-equal-p 1.0 (magnitude (normalize v)))) t)

(deftest tup-17
(let ((a (make-vec 1 2 3))
      (b (make-vec 2 3 4)))
  (equal 20.0 (dot a b))) t)

(deftest tup-18
(let ((a (make-vec 1 2 3))
      (b (make-vec 2 3 4)))
  (and
   (equalp (cross a b) (make-vec -1 2 -1))
   (equalp (cross b a) (make-vec 1 -2 1)))) t)

(deftest reflect-vector
  (let* ((v (make-vec 1 -1 0))
	 (n (make-vec 0 1 0))
	 (r (reflect v n)))
    (equalp r (make-vec 1 1 0))) t)

(deftest reflect-vector-slanted
  (let* ((v (make-vec 0 -1 0))
	 (sqrt-2-over-2 (/ (sqrt 2) 2))
	 (n (make-vec sqrt-2-over-2 sqrt-2-over-2 0))
	 (r (reflect v n)))
    (tuples-equalp r (make-vec 1 0 0))) t)

