(in-package :com.godel.intersections)

(progn
  (load "p:/scripts/lisp/ray/rt.lisp")
  (load "p:/scripts/lisp/ray/spheres.lisp"))

(deftest intersection-1
    (let* ((s (spheres:make-sphere))
	   (i (make-intersection 3.5 s)))
      (and
       (= (sint-t-val i) 3.5)
       (equalp (sint-obj i) s))) t)

(deftest aggregate-intersections
  (let* ((s (spheres:make-sphere))
	 (i1 (make-intersection 1 s))
	 (i2 (make-intersection 2 s))
	 (xs (intersections i1 i2)))
    (and
     (= (length xs) 2)
     (= (sint-t-val (first xs)) 1)
     (= (sint-t-val (second xs)) 2))) t)

(deftest intersect-set
  (let* ((r (make-ray (make-point 0 0 -5) (make-vec 0 0 1)))
	 (s (spheres:make-sphere))
	 (xs (spheres:intersect s r)))
    (and
     (= (length xs) 2)
     (equalp (sint-obj (first xs)) s)
     (equalp (sint-obj (second xs)) s))) t)

(deftest hit-all-positive-t-vals
  (let* ((s (spheres:make-sphere))
	 (i1 (make-intersection 1 s))
	 (i2 (make-intersection 2 s))
	 (xs (intersections i2 i1))
	 (i (hit xs)))
    (equalp i i1)) t)

(deftest hit-some-negative-t-vals
  (let* ((s (spheres:make-sphere))
	 (i1 (make-intersection -1 s))
	 (i2 (make-intersection 1 s))
	 (xs (intersections i2 i1))
	 (i (hit xs)))
    (equalp i i2)) t)

(deftest hit-all-negative-t-vals
  (let* ((s (spheres:make-sphere))
	 (i1 (make-intersection -2 s))
	 (i2 (make-intersection -1 s))
	 (xs (intersections i2 i1))
	 (i (hit xs)))
    (null i)) t)

(deftest hit-always-lowest-non-neg
  (let* ((s (spheres:make-sphere))
	 (i1 (make-intersection 5 s))
	 (i2 (make-intersection 7 s))
	 (i3 (make-intersection -3 s))
	 (i4 (make-intersection 2 s))
	 (xs (intersections i1 i2 i3 i4))
	 (i (hit xs)))
    (equalp i i4)) t)

  


