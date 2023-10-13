(in-package :com.godel.transform)

(load "p:/scripts/lisp/ray/tuples.lisp")
(load "p:/scripts/lisp/ray/matrix.lisp")


(defun translation (x y z)
  (make-array '(4 4) :initial-contents `((1 0 0 ,x)
					 (0 1 0 ,y)
					 (0 0 1 ,z)
					 (0 0 0 1))))

(defun scaling (x y z)
  "Return a scaling matrix"
  (make-array '(4 4) :initial-contents `((,x 0 0 0)
					 (0 ,y 0 0)
					 (0 0 ,z 0)
					 (0 0 0 1))))

(defun rotation-x (r)
  "A transformation matrix for rotation R radians around the x-axis."
  (make-array '(4 4) :initial-contents `((1 0 0 0)
					 (0 ,(cos r) ,(- (sin r)) 0)
					 (0 ,(sin r) ,(cos r) 0)
					 (0 0 0 1))))

(defun rotation-y (r)
  "A transformation matrix for rotation R radians around the y-axis."
  (make-array '(4 4) :initial-contents `((,(cos r) 0 ,(sin r) 0)
					 (0 1 0 0)
					 (,(- (sin r)) 0 ,(cos r) 0)
					 (0 0 0 1))))

(defun rotation-z (r)
  "A transformation matrix for rotation R radians around the z-axis."
  (make-array '(4 4) :initial-contents `((,(cos r) ,(- (sin r)) 0 0)
					 (,(sin r) ,(cos r) 0 0)
					 (0 0 1 0)
					 (0 0 0 1))))

(defun shearing (x-y x-z y-x y-z z-x z-y)
  (make-array '(4 4) :initial-contents `((1 ,x-y ,x-z 0)
					 (,y-x 1 ,y-z 0)
					 (,z-x ,z-y 1 0)
					 (0 0 0 1))))
