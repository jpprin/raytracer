(in-package :com.greylock.world)

(progn
  (load "p:/scripts/lisp/ray/rt.lisp")
  (load "P:/scripts/lisp/ray/tuples.lisp")
  (load "P:/scripts/lisp/ray/colors.lisp")
  (load "P:/scripts/lisp/ray/transform.lisp")
  (load "P:/scripts/lisp/ray/rays.lisp")
  (load "P:/scripts/lisp/ray/materials.lisp")
  (load "P:/scripts/lisp/ray/spheres.lisp")
  (load "p:/scripts/lisp/ray/lights.lisp")
  (load "p:/scripts/lisp/ray/world.lisp")
  )

(deftest create-world-test
  (let ((w (make-world)))
    (and
     (null (world-objects w))
     (null (world-light w)))) t)

(deftest default-world-test
  (let ((light (point-light (make-point -10 10 -10) *WHITE*))
	(s1 (make-sphere :material
			 (make-material
			  :color (create-color 0.8 1.0 0.6)
			  :diffuse 0.7
			  :specular 0.2)))
	(s2 (make-sphere :transform (scaling 0.5 0.5 0.5)))
	(w (default-world)))
    (and
     (equalp (world-light w) light)
     (member s1 (world-objects w) :test #'equalp)
     (member s2 (world-objects w) :test #'equalp)
     t)) ; throwing in a final T just so the whole AND expression returns T
  t)

;; (deftest intersect-world-with-ray
;;   (let* ((w (default-world))
;; 	(r (make-ray (make-point 0 0 -5) (make-vec 0 0 1)))
;; 	 (xs (intersect-world w r)))
;;     (length (first xs)))

    ;; (and
    ;;  (= 4 (length xs))
    ;;  (= 4 (first xs))
    ;;  (= 4.5 (second xs))
    ;;  (= 5.5 (third xs))
    ;;  (= 6 (fourth xs)))))



   
