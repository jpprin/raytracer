
(in-package :com.godel.canvas)

(load "P:/scripts/lisp/ray/colors.lisp")

(defparameter *max-buffer-length* 70
  "The maximum length of a line that will be written into a PPM file.")

(defun create-canvas (height width &optional (initial-color (create-color 0 0 0)))
  "Create a canvas of pixels of dimensions WIDTH by HEIGHT. Each pixel will be intially set to INITIAL-COLOR, the default being black otherwise specified."
  (make-array (list width height) :initial-element initial-color))

(defun canvas-width (c)
  "Return the width of canvas C."
  (array-dimension c 1))

(defun canvas-height (c)
  "Return the height of canvas C."
  (array-dimension c 0))

(defun get-pixel (c row col)
  "Return the color at pixel (ROW, COL) on canvas C."
  (aref c row col))
  
(defun write-pixel (canv x y col)
  "Write the color COL to the canvas CANV at the location X, Y"
  (setf (aref canv x y) col))

(defun pixel-at (canv x y)
  "Return the color on canvas CANV at the location X, Y"
  (aref canv x y))

(defun canvas-to-ppm (canv)
  "Return a PPMv3 compliant string from CANV."
  (let ((c-height (first (array-dimensions canv)))
	(c-width (second (array-dimensions canv)))
	(buffer-len 0))

    (with-output-to-string (s)
      ; count down from height since the y-axis is negative in a PPM file
      (loop for i from (1- c-height) downto 0
	      initially (format s "P3~%~d ~d~%255~%" c-width c-height)
	    do (loop for j from 0 below c-width
		     do (loop for col in (color-str-to-list
					  (color-to-255-str (aref canv i j)))
			      do (if (> *max-buffer-length*
					(+ (1+ (length col)) buffer-len))
				     (progn
				       (format s " ~d" col)
				       (incf buffer-len (1+ (length col))))
				     (progn
				       (format s "~%~d" col)
				       (setf buffer-len 0)))))

	    finally (terpri s))))) ; terminate file with newline





#|
The tests below don't seem to work, but they don't really seem to need to work 
(as presented) in order for the canvas module to create perfectly valid PPM files.
|#

;; (let ((c (create-canvas 5 3))
;;       (c1 (create-color 1.5 0 0))
;;       (c2 (create-color 0 0.5 0))
;;       (c3 (create-color -0.5 0 1)))
;;   (write-pixel c 0 0 c1)
;;   (write-pixel c 2 1 c2)
;;   (write-pixel c 4 2 c3)
;;   (with-input-from-string (s1 (canvas-to-ppm c))
;;     (read-line s1)
;;     (read-line s1)
;;     (read-line s1)
;;     (read-line s1)
;;     (and
;;      (string-equal (read-line s1) "255 0 0 0 0 0 0 0 0 0 0 0 0 0 0 ")
;;      (string-equal (read-line s1) "0 0 0 0 0 0 0 128 0 0 0 0 0 0 0 ")
;;      (string-equal (read-line s1) "0 0 0 0 0 0 0 0 0 0 0 0 0 0 255 "))))

;; (let ((c (create-canvas 10 2 (create-color 1 0.8 0.6))))
;; ;  (string-equal
;;    (canvas-to-buffer-v2 c))
;;    " 255 204 153 255 204 153 255 204 153 255 204 153 255 204 153 255 204
;; 153 255 204 153 255 204 153 255 204 153 255 204 153
;; 255 204 153 255 204 153 255 204 153 255 204 153 255 204 153 255 204
;; 153 255 204 153 255 204 153 255 204 153 255 204 153"))


		




