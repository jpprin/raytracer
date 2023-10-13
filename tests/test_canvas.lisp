
(in-package :com.godel.canvas)

(progn
  (load "P:/scripts/lisp/ray/rt.lisp")
  (load "P:/scripts/lisp/ray/canvas.lisp"))

(deftest canvas-1
    (let ((c1 (create-canvas 10 20)))
      (and
       (= 10 (canvas-width c1))
       (= 20 (canvas-height c1))
       (color-black-p (get-pixel c1 1 1)))) t)

(deftest canvas-2
    (let ((c (create-canvas 10 20))
	  (red (create-color 1 0 0)))
      (progn
	(write-pixel c 2 3 red)
	(color-red-p (pixel-at c 2 3)))) t)

(deftest canvas-3
    (let ((c (create-canvas 5 3)))
      (with-input-from-string (ppm (canvas-to-ppm c))
	(and
	 (equal (read-line ppm) "P3")
	 (equal (read-line ppm) "5 3")
	 (equal (read-line ppm) "255")))) t)

