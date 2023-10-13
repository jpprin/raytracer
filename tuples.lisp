
(in-package :com.godel.tuples)

;; OLD DEFINITIONS. I'M TRYING THE NEW ONES BELOW FOR THE TIME BEING.
;; 
;; (defstruct (tuple (:type vector) (:constructor make-tuple (x y z w)))
;;   "An ordered list of floats that will be used to create POINTS and VECTORS."
;;   x y z w)

;; (defun tuple-p (tup)
;;   (equal (type-of tup) '(SIMPLE-VECTOR 4)))

;; (defstruct (point (:type vector) (:include tuple (w 1.0))
;; 		  (:constructor make-point (x y z)))
;;   "A POINT is a type of TUPLE where the W coordinate is set to 1.0")

;; (defun is-point (p)
;;   (and
;;    (equal (type-of p) '(SIMPLE-VECTOR 4))
;;    (= 1.0 (point-w p))))

;; (defstruct (vec (:type vector) (:include tuple (w 0.0))
;; 		(:constructor make-vec (x y z)))
;;   "A VEC is a type of TUPLE where the W coordinate is set to 0.0.")

;; (defun is-vec (v)
;;   (and
;;    (equal (type-of v) '(SIMPLE-VECTOR 4))
;;    (= 0.0 (vec-w v))))

;=====================================================================

(defun make-tuple (x y z w)
  (make-array '(4 1) :initial-contents `((,x) (,y) (,z) (,w))))

(defun tuple-x (tup)
  (aref tup 0 0))

(defun tuple-y (tup)
  (aref tup 1 0))

(defun tuple-z (tup)
  (aref tup 2 0))

(defun tuple-w (tup)
  (aref tup 3 0))

(defun make-point (x y z)
  (make-tuple x y z 1.0))

(defun point-x (pt)
  (aref pt 0 0))

(defun point-y (pt)
  (aref pt 1 0))

(defun point-z (pt)
  (aref pt 2 0))

(defun point-w (pt)
  (aref pt 3 0))

(defun is-point (p)
  (and
   (equal (type-of p) '(SIMPLE-ARRAY T (4 1)))
   (= 1.0 (point-w p))))

(defun make-vec (x y z)
  (make-tuple x y z 0.0))

(defun vec-x (pt)
  (aref pt 0 0))

(defun vec-y (pt)
  (aref pt 1 0))

(defun vec-z (pt)
  (aref pt 2 0))

(defun vec-w (pt)
  (aref pt 3 0))

(defun is-vec (p)
  (and
   (equal (type-of p) '(SIMPLE-ARRAY T (4 1)))
   (= 0.0 (vec-w p))))

;=====================================================================

(defun floats-equal-p (n m)
  (let ((epsilon 0.00001))
    (< (abs (- m n)) epsilon)))

(defun tuples-equalp (tup1 tup2)
  (and
   (floats-equal-p (tuple-x tup1) (tuple-x tup2))
   (floats-equal-p (tuple-y tup1) (tuple-y tup2))
   (floats-equal-p (tuple-z tup1) (tuple-z tup2))
   (floats-equal-p (tuple-w tup1) (tuple-w tup2))))

(defun tuple-arithmetic (tup1 tup2 op)
  "A higher order function to do arithmetic on two tuples."
  (make-tuple (funcall op  (tuple-x tup1) (tuple-x tup2))
	      (funcall op  (tuple-y tup1) (tuple-y tup2))
	      (funcall op  (tuple-z tup1) (tuple-z tup2))
	      (funcall op  (tuple-w tup1) (tuple-w tup2))))

(defun add-tuples (tup1 tup2)
  "Return a TUPLE that is the sum of TUP1 and TUP2"
  (tuple-arithmetic tup1 tup2 #'+))

(defun subtract-tuples (tup1 tup2)
  "Return a TUPLE that is the difference between TUP1 and TUP2."
  (tuple-arithmetic tup1 tup2 #'-))

(defun negate-tuple (tup)
  (make-tuple (- (tuple-x tup))
	      (- (tuple-y tup))
	      (- (tuple-z tup))
	      (- (tuple-w tup))))

(defun scalar-multiply (tup scalar)
  (make-tuple (* (tuple-x tup) scalar)
	      (* (tuple-y tup) scalar)
	      (* (tuple-z tup) scalar)
	      (* (tuple-w tup) scalar)))

(defun scalar-divide (tup scalar)
  (make-tuple (/ (tuple-x tup) scalar)
	      (/ (tuple-y tup) scalar)
	      (/ (tuple-z tup) scalar)
	      (/ (tuple-w tup) scalar)))

(defun magnitude (v)
  (sqrt (+ (expt (tuple-x v) 2)
	   (expt (tuple-y v) 2)
	   (expt (tuple-z v) 2)
	   (expt (tuple-w v) 2))))

(defun normalize (v)
  "Return a unit vector from VEC"
  (let ((mag (magnitude v)))
    (scalar-divide v mag)))

(defun dot (tup1 tup2)
  "Return the dot product of TUP1 and TUP2"
  (+ (* (tuple-x tup1) (tuple-x tup2))
     (* (tuple-y tup1) (tuple-y tup2))
     (* (tuple-z tup1) (tuple-z tup2))
     (* (tuple-w tup1) (tuple-w tup2))))

(defun cross (a b)
  "Return the cross product of A and B."
  (make-vec
   ; x component
   (- (* (tuple-y a) (tuple-z b))
      (* (tuple-z a) (tuple-y b)))
   ; y component
   (- (* (tuple-z a) (tuple-x b))
      (* (tuple-x a) (tuple-z b)))
   ; z component
   (- (* (tuple-x a) (tuple-y b))
      (* (tuple-y a) (tuple-x b)))))

(defun reflect (in normal)
  "Return the result of reflecting the IN vector around the NORMAL vector."
  (subtract-tuples in (scalar-multiply normal (* 2 (dot in normal)))))
  
