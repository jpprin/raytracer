(in-package :com.godel.firstrender)

(progn
;  (load "P:/scripts/lisp/ray/tuples.lisp")  
;  (load "P:/scripts/lisp/ray/colors.lisp")
  (load "P:/scripts/lisp/ray/canvas.lisp")
;  (load "P:/scripts/lisp/ray/matrix.lisp")
  (load "P:/scripts/lisp/ray/transform.lisp")
;  (load "P:/scripts/lisp/ray/rays.lisp")
  (load "P:/scripts/lisp/ray/intersections.lisp")
  (load "P:/scripts/lisp/ray/materials.lisp")
  (load "P:/scripts/lisp/ray/spheres.lisp")
  (load "P:/scripts/lisp/ray/lights.lisp")  
  )

(setf *ray-origin* (make-point 0 0 -5)
      *wall-z* 10 ; original value 10
      *wall-size* 7.0
      *canvas-pixels* 100
      *pixel-size* (/ *wall-size* *canvas-pixels*)
      *half* (/ *wall-size* 2)
      *s1* (make-sphere
	    ; the below can change the shape of the sphere
	    ;:transform (shearing 1 0 0 0 0 0)
	    :material (make-material
		       :color (create-color 1 0.2 1)
		       ; lower the diffuse to see what happens
		       :diffuse 0.95
		       ; lower the shininess to make it more shiny
		       :shininess 100.0))
      *light* (point-light (make-point -10 10 -5) *WHITE*)
      )

(defun make-sphere-shadow (s)
  (let ((canvas (create-canvas *canvas-pixels* *canvas-pixels*))
	(shape s))
    (do* ((y 0 (1+ y))
	  (world-y (- *half* (* *pixel-size* y)) (- *half* (* *pixel-size* y))))
	 ((= y (1- *canvas-pixels*)) canvas)
      (do* ((x 0 (1+ x))
	    (world-x (+ (- *half*) (* x *pixel-size*))
		     (+ (- *half*) (* x *pixel-size*)))
	    (position (make-point world-x world-y *wall-z*)
		      (make-point world-x world-y *wall-z*))
	    (r (make-ray *ray-origin*
			 (normalize (subtract-tuples position *ray-origin*)))
	       (make-ray *ray-origin*
			 (normalize (subtract-tuples position *ray-origin*))))
	    (xs (intersect shape r) (intersect shape r)))
	   ((= x (1- *canvas-pixels*)))
	(if (hit xs)
	    (let* ((hit-point (ray-position r (sint-t-val (hit xs))))
		   (nv (normal-at s (make-point x y *wall-z*))) ; normal vector
		   (eye (scalar-multiply (ray-direction r) -1)) ; eye vector
		   (point-color (lighting (sphere-material s)
					  *light* hit-point eye nv)))
	      (write-pixel canvas x y point-color)))))))
		  


; the below creates a PPM file, so uncomment it to run

(with-open-file (stream "p:/scripts/lisp/ray/images/firstrender.ppm"
			:direction :output :if-exists :supersede)
  (format stream
	  (canvas-to-ppm
	   (make-sphere-shadow *s1*))))


  
