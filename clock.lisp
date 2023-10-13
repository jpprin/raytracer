(in-package :com.godel.clock)

(progn
  (load "P:/scripts/lisp/ray/canvas.lisp")
  (load "P:/scripts/lisp/ray/transform.lisp")
  (load "P:/scripts/lisp/ray/colors.lisp"))

(defvar *ORIGIN* (make-point 0 0 0) "Center of the clock")
(defvar *NOON*   (make-point 0 0 1) "Representation of 12 noon")
(defvar *HOUR-ANGLE* (/ pi 6) "Angle that corresponds to one hour")


(defun hours-from-noon (hours)
  "Return a point that is angled HOURS away from NOON.  This assumes a clock that is oriented along the y axies."
  (let ((r (* hours *HOUR-ANGLE*)))
    (mmul (rotation-y r) *NOON*)))

(defun align-point (pt clock-radius canvas-height)
  "Adjust the point PT to line up with the appropriate spot on the CANVAS-HEIGHT.  Multiply the X and Y coordinates by the CLOCK-RADIUS (ie, scaling) then move it to the center of the canvas by adding the coordinates of the center point (ie, movement)."
  (let ((movement-transform (translation canvas-height 0 canvas-height))
	(scaling-transform (scaling clock-radius clock-radius clock-radius)))
    (mmul movement-transform (mmul scaling-transform pt))))
  
(defun draw-clock (canvas-size)
  "Creates a canvas of size CANVAS-SIZE and draws 12 white points on it that correspond to the hours of a clock."
  (let* ((canv (create-canvas canvas-size canvas-size))
	 (clock-radius (* 0.4 (canvas-width canv)))) ; arbitrarily choose clock radius
    (loop for hour from 0 to 11
	  for time-dot = (hours-from-noon hour) then (hours-from-noon hour)
	  for new-pt = (align-point time-dot clock-radius (/ (canvas-height canv) 2))
	    then (align-point time-dot clock-radius (/ (canvas-height canv) 2))
	  do (write-pixel canv (floor (point-z new-pt))
			  (floor (point-x new-pt)) *WHITE*))
    canv))
    
; the below creates a PPM file, so uncomment it to run

;; (with-open-file (stream "p:/scripts/lisp/ray/images/clock.ppm"
;; 			:direction :output :if-exists :supersede)
;;   (format stream
;; 	  (canvas-to-ppm
;; 	    (draw-clock 100))))
