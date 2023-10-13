
(in-package :com.godel.lights)

(progn
  (load "p:/scripts/lisp/ray/rt.lisp")
  (load "p:/scripts/lisp/ray/tuples.lisp")
  (load "p:/scripts/lisp/ray/colors.lisp")  
  (load "p:/scripts/lisp/ray/spheres.lisp")
  (load "p:/scripts/lisp/ray/lights.lisp"))


(deftest create-point-light
  (let* ((intensity *WHITE*)
	 (position (make-point 0 0 0))
	 (light (point-light position intensity)))
    (and
     (equalp (light-position light) position)
     (equalp (light-intensity light) intensity))) t)

; (do-test 'create-light)
