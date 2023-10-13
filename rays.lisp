(in-package :com.godel.rays)

(load "p:/scripts/lisp/ray/tuples.lisp")
(load "p:/scripts/lisp/ray/matrix.lisp")

(defstruct (ray (:constructor make-ray (origin direction)))
	     origin direction)

(defun ray-position (r u)
  "Return the position of ray R after U units of time.  The time variable can't be T because that is already the built-in truth value in Lisp."
  (add-tuples
   (ray-origin r)
   (scalar-multiply (ray-direction r) u)))

(defun transform (a-ray a-matrix)
  "Apply a transformation matrix A-MATRIX to A-RAY.  Return a new RAY object."
  (make-ray
   (mmul a-matrix (ray-origin a-ray))
   (mmul a-matrix (ray-direction a-ray))))
