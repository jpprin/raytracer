(in-package :com.godel.silhouette)

(progn
  (load "P:/scripts/lisp/ray/tuples.lisp")  
  (load "P:/scripts/lisp/ray/colors.lisp")
  (load "P:/scripts/lisp/ray/canvas.lisp")
  (load "P:/scripts/lisp/ray/matrix.lisp")
  (load "P:/scripts/lisp/ray/transform.lisp")
  (load "P:/scripts/lisp/ray/rays.lisp")
  (load "P:/scripts/lisp/ray/intersections.lisp")
  (load "P:/scripts/lisp/ray/spheres.lisp"))

(setf *ray-origin* (make-point 0 0 -5)
      *wall-z* 10
      *wall-size* 7.0
      *canvas-pixels* 100
      *pixel-size* (/ *wall-size* *canvas-pixels*)
      *half* (/ *wall-size* 2))

(defun make-sphere-shadow (s)
  (let ((canvas (create-canvas *canvas-pixels* *canvas-pixels*))
	(color *RED*)
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
	(if (hit xs) (write-pixel canvas x y color))))))


; the below creates a PPM file, so uncomment it to run

;; (with-open-file (stream "p:/scripts/lisp/ray/images/silhouette.ppm"
;; 			:direction :output :if-exists :supersede)
;;   (format stream
;; 	  (canvas-to-ppm
;; 	   (make-sphere-shadow
;; 	    (make-sphere
;; 	     :transform (mmul (shearing 1 0 0 0 0 0) (scaling 0.5 1 1)))))))

	   
    
	    
