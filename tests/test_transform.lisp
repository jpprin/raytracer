
(in-package :com.godel.transform)

(progn
  (load "p:/scripts/lisp/ray/rt.lisp")
  (load "p:/scripts/lisp/ray/transform.lisp"))

(deftest transform-1
  (let ((transform (translation 5 -3 2))
      (p (make-point -3 4 5)))
    (equalp (mmul transform p) (make-point 2 1 7))) t)

(deftest transform-2
  (let* ((transform (translation 5 -3 2))
       (inv (inverse transform))
       (p (make-point -3 4 5)))
    (equalp (mmul inv p) (make-point -8 7 3))) t)

(deftest transform-3
    (let ((transform (translation 5 -3 2))
      (v (make-vec -3 4 5)))
      (equalp (mmul transform v) v)) t)

(deftest transform-4
  (let ((transform (scaling 2 3 4))
	(p (make-point -4 6 8)))
    (matrix-equalp (mmul transform p) (make-point -8 18 32))) t)

(deftest transform-5
  (let ((transform (scaling 2 3 4))
	(v (make-vec -4 6 8)))
    (matrix-equalp (mmul transform v) (make-vec -8 18 32))) t)

(deftest transform-6
  (let* ((transform (scaling 2 3 4))
	 (inv (inverse transform))
	 (v (make-vec -4 6 8)))
    (matrix-equalp (mmul inv v) (make-vec -2 2 2))) t)

(deftest reflection-scaling
  (let ((transform (scaling -1 1 1))
	(p (make-point 2 3 4)))
    (matrix-equalp (mmul transform p) (make-point -2 3 4))) t)

(deftest rotate-x-axis
  (let ((p (make-point 0 1 0))
	(half-quarter (rotation-x (/ pi 4)))
	(full-quarter (rotation-x (/ pi 2))))
    (matrix-equalp (mmul half-quarter p) (make-point 0 (/ (sqrt 2) 2) (/ (sqrt 2) 2)))) t)

(deftest inverse-rotate-x-axis
  (let* ((p (make-point 0 1 0))
	(half-quarter (rotation-x (/ pi 4)))
	 (inv (inverse half-quarter)))
    (matrix-equalp (mmul inv p) (make-point 0 (/ (sqrt 2) 2) (- (/ (sqrt 2) 2))))) t)

(deftest rotate-y-axis
  (let ((p (make-point 0 0 1))
	(half-quarter (rotation-y (/ pi 4)))
	(full-quarter (rotation-y (/ pi 2))))
    (and
     (matrix-equalp (mmul half-quarter p) (make-point (/ (sqrt 2) 2) 0 (/ (sqrt 2) 2)))
     (matrix-equalp (mmul full-quarter p) (make-point 1 0 0)))) t)

(deftest rotate-z-axis
  (let ((p (make-point 0 1 0))
	(half-quarter (rotation-z (/ pi 4)))
	(full-quarter (rotation-z (/ pi 2))))
    (and
     (matrix-equalp (mmul half-quarter p) (make-point (- (/ (sqrt 2) 2)) (/ (sqrt 2) 2) 0))
     (matrix-equalp (mmul full-quarter p) (make-point -1 0 0)))) t)

(deftest shearing-1
  (let ((transform (shearing 1 0 0 0 0 0))
	(p (make-point 2 3 4)))
    (matrix-equalp (mmul transform p) (make-point 5 3 4))) t)

(deftest shearing-2
  (let ((transform (shearing 0 1 0 0 0 0 ))
	(p (make-point 2 3 4)))
    (matrix-equalp (mmul transform p) (make-point 6 3 4))) t)

(deftest shearing-3
  (let ((transform (shearing 0 0 1 0 0 0))
	(p (make-point 2 3 4)))
    (matrix-equalp (mmul transform p) (make-point 2 5 4))) t)

(deftest shearing-4
  (let ((transform (shearing 0 0 0 1 0 0))
	(p (make-point 2 3 4)))
    (matrix-equalp (mmul transform p) (make-point 2 7 4))) t)

(deftest shearing-5
  (let ((transform (shearing 0 0 0 0 1 0))
	(p (make-point 2 3 4)))
    (matrix-equalp (mmul transform p) (make-point 2 3 6))) t)

(deftest shearing-6
  (let ((transform (shearing 0 0 0 0 0 1))
	(p (make-point 2 3 4)))
    (matrix-equalp (mmul transform p) (make-point 2 3 7))) t)

(deftest sequential-transformations
    (let* ((p (make-point 1 0 1))
	   (A (rotation-x (/ pi 2)))
	   (B (scaling 5 5 5))
	   (C (translation 10 5 7))
	   (p2 (mmul A p))
	   (p3 (mmul B p2))
	   (p4 (mmul C p3)))
      (and
       (matrix-equalp p2 (make-point 1 -1 0))
       (matrix-equalp p3 (make-point 5 -5 0))
       (matrix-equalp p4 (make-point 15 0 7)))) t)

(deftest chained-transformation
  (let* ((p (make-point 1 0 1))
	 (A (rotation-x (/ pi 2)))
	 (B (scaling 5 5 5))
	 (C (translation 10 5 7))
	 (transformation (mmul C (mmul B A))))
    (matrix-equalp (mmul transformation p) (make-point 15 0 7))) t)






