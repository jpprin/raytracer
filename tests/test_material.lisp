(in-package :com.godel.materials)

(progn
  (load "p:/scripts/lisp/ray/rt.lisp")
  (load "p:/scripts/lisp/ray/tuples.lisp")
  (load "p:/scripts/lisp/ray/colors.lisp")  
  (load "p:/scripts/lisp/ray/materials.lisp")
    (load "p:/scripts/lisp/ray/lights.lisp")
  )

(deftest default-material
    (let ((m (make-material)))
      (and
       (equal (material-color m) *WHITE*)
       (= (material-ambient m) 0.1)
       (= (material-diffuse m) 0.9)
       (= (material-specular m) 0.9)
       (= (material-shininess m) 200.0))) t)

(deftest eye-between-light-and-surface
  (let* ((m (make-material))
	 (position (make-point 0 0 0))
	 (eyev (make-vec 0 0 -1))
	 (normalv (make-vec 0 0 -1))
	 (light (lights:point-light (make-point 0 0 -10) *WHITE*))
	 (result (lighting m light position eyev normalv)))
    (colors-equal-p result (create-color 1.9 1.9 1.9))) t)


(deftest |eye between light and surface offset 45 degrees|
  (let* ((m (make-material))
	 (position (make-point 0 0 0))
	 (sqrt-2-over-2 (/ (sqrt 2) 2))
	 (eyev (make-vec 0 sqrt-2-over-2 sqrt-2-over-2))
	 (normalv (make-vec 0 0 -1))
	 (light (lights:point-light (make-point 0 0 -10) *WHITE*))
	 (result (lighting m light position eyev normalv)))
    (colors-equal-p result (create-color 1.0 1.0 1.0))) t)

(deftest |lighting with eye opposite surface, light offset 45 degrees|
  (let* ((m (make-material))
	 (position (make-point 0 0 0))
	 (eyev (make-vec 0 0 -1))
	 (normalv (make-vec 0 0 -1))
	 (light (lights:point-light (make-point 0 10 -10) *WHITE*))
	 (result (lighting m light position eyev normalv)))
    (colors-equal-p result (create-color 0.7364 0.7364 0.7364))) t)

(deftest |lighting with eye in the past of the reflection vector|
  (let* ((m (make-material))
	 (position (make-point 0 0 0))
	 (sqrt-2-over-2 (/ (sqrt 2) 2))
	 (eyev (make-vec 0 (- sqrt-2-over-2) (- sqrt-2-over-2)))
	 (normalv (make-vec 0 0 -1))
	 (light (lights:point-light (make-point 0 10 -10) *WHITE*))
	 (result (lighting m light position eyev normalv)))
    (colors-equal-p result (create-color 1.63639 1.63639 1.63639))) t)


(deftest |lighting with the light behind the surface|
  (let* ((m (make-material))
	 (position (make-point 0 0 0))
	 (eyev (make-vec 0 0 -1))
	 (normalv (make-vec 0 0 -1))
	 (light (lights:point-light (make-point 0 0 10) *WHITE*))
	 (result (lighting m light position eyev normalv)))
    (colors-equal-p result (create-color 0.1 0.1 0.1))) t)
  


  
	    
	
      


     
