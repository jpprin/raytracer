(in-package :com.godel.spheres)

(progn
  (load "p:/scripts/lisp/ray/tuples.lisp")
  (load "p:/scripts/lisp/ray/rays.lisp")
  (load "p:/scripts/lisp/ray/matrix.lisp")
  (load "p:/scripts/lisp/ray/intersections.lisp")
  (load "p:/scripts/lisp/ray/materials.lisp"))

(defstruct sphere (transform idm) (material (make-material)))

(defun intersect (a-sphere a-ray)
  "Return a list of the intersection objects of a ray A-RAY and a sphere A-SPHERE.  If the ray does not hit the sphere, return NIL.  If the ray is a tangent, a list of two objects containing identical t-values is returned."
  (let* ((ray2 (rays:transform a-ray (inverse (sphere-transform a-sphere))))
	 (sphere-to-ray (subtract-tuples (ray-origin ray2) (make-point 0 0 0)))
	 (a (dot (ray-direction ray2) (ray-direction ray2)))
	 (b (* 2 (dot (ray-direction ray2) sphere-to-ray)))
	 (c (1- (dot sphere-to-ray sphere-to-ray)))
	 (discriminant (- (* b b) (* 4 a c))))
    (unless (minusp discriminant)
      (mapcar #'(lambda (t-val) (make-intersection t-val a-sphere))
	      (sort ; intersections should be returned in increasing order
	       (list
		(/ (- (- b) (sqrt discriminant)) (* 2 a))  ; t1
		(/ (+ (- b) (sqrt discriminant)) (* 2 a))) ; t2
	       #'<)))))

(defun set-transform (a-sphere a-matrix)
  "Assign the transformation matrix A-MATRIX to A-SPHERE's tranform slot."
  (setf (sphere-transform a-sphere) a-matrix))

(defun normal-at (sphere world-point)
  "Return a surface normal vector for a point P on the surface of the SPHERE by extending a vector from the origin of SPHERE out to point P."
  (let* ((object-point (mmul (inverse (sphere-transform sphere)) world-point))
	 (object-normal (subtract-tuples object-point (make-point 0 0 0)))
	 (world-normal (mmul (transpose-4x4 (inverse (sphere-transform sphere)))
			     object-normal)))
    (setf (aref world-normal 3 0) 0)
    (normalize world-normal)))

  

