
(in-package :com.godel.rays)

(progn
  (load "p:/scripts/lisp/ray/rt.lisp")
  (load "p:/scripts/lisp/ray/matrix.lisp")  
  (load "p:/scripts/lisp/ray/transform.lisp")
  (load "p:/scripts/lisp/ray/rays.lisp"))

(deftest ray-1
    (let* ((origin (make-point 1 2 3))
	   (direction (make-vec 4 5 6))
	   (r (make-ray :origin origin :direction direction)))
      (and
       (equalp (ray-origin r) origin)
       (equalp (ray-direction r) direction))) t)

(deftest ray-2
    (let ((r (make-ray :origin (make-point 2 3 4) :direction (make-vec 1 0 0))))
      (and
       (equalp (ray-position r 0) (make-point 2 3 4))
       (equalp (ray-position r 1) (make-point 3 3 4))
       (equalp (ray-position r -1) (make-point 1 3 4))
       (equalp (ray-position r 2.5) (make-point 4.5 3 4)))) t)

(deftest translate-ray
  (let* ((r (make-ray (make-point 1 2 3) (make-vec 0 1 0)))
	 (m (translation 3 4 5))
	 (r2 (transform r m)))
    (and
     (equalp (ray-origin r2) (make-point 4 6 8))
     (equalp (ray-direction r2) (make-vec 0 1 0)))) t)

(deftest scaling-ray
  (let* ((r (make-ray (make-point 1 2 3) (make-vec 0 1 0)))
	 (m (scaling 2 3 4))
	 (r2 (transform r m)))
    (and
     (equalp (ray-origin r2) (make-point 2 6 12))
     (equalp (ray-direction r2) (make-vec 0 3 0)))) t)



