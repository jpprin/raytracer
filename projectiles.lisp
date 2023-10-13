; PROJECTILES PROJECT

(in-package :com.godel.projectiles)

(load "p:/scripts/lisp/ray/tuples.lisp")
(load "p:/scripts/lisp/ray/canvas.lisp")


; THE REST OF THE CODE

(defstruct projectile position velocity)
(defstruct environment gravity wind)

(defvar p1 (make-projectile
	    :position (tuples:make-vector 0 1 0)
	    :velocity (tuples:normalize
		       (tuples:make-vector 1 1 0))))

(defvar e1 (make-environment
	    :gravity (tuples:make-vector 0 -0.1 0)
	    :wind (tuples:make-vector -0.01 0 0)))




(defun tick (env proj)
  "Return a new projectile after one 'tick' (ie, unit of time)."
  (make-projectile
   :position (add-tuples (projectile-position proj)
			 (projectile-velocity proj))
   :velocity (add-tuples (projectile-velocity proj)
			 (add-tuples (environment-gravity env)
				     (environment-wind env)))))

(defun get-projectile-x (proj)
  "Return the X axis of the projectile."
  (com.godel.tuples:tuple-x (projectile-position proj)))

(defun get-projectile-y (proj)
  "Return the Y axis of the projectile."
  (com.godel.tuples:tuple-y (projectile-position proj)))


(defun report-projectile-height-helper (proj env tries)
  (format t "~%Projectile coordinates are now (~d, ~d)"
	  (get-projectile-x proj) (get-projectile-y proj))
    (cond
      ((> tries 50) (format t "~%Projectile is out of sight!"))
      ((< (get-projectile-y proj) 0) (format t "~%LANDED"))
      (t (report-projectile-height-helper (tick env proj) env (1+ tries)))))


(defun report-projectile-height (proj env)
  (report-projectile-height-helper proj env 0))


(defun draw-projectile-path (canvas projectile env)
  "Return a canvas that shows the path of PROJECTILE written on CANVAS, given teh environment ENV.  The default color for now is red, but this should be changed to allow the user to define the color of the path.

TODO:
  1) The default color is red. Change this to a user-defined color.
  2) The projectile runs along the top of the canvas in a straight line if the
     the path exceeds the canvas height. This should be changed so that it just
     'disappears from view', then reappears as it comes down.
  3) The y-path needs to be converted to negative because now the image is
     upside-down
  4) The program doesn't know what to do on a small canvas
  5) the ATTEMPT variable can eventually be removed, its just to prevent inf. loops"
  (loop for attempt from 1 upto 50
	for canv = canvas
	for proj = projectile then (tick env proj)
	for px = (get-projectile-x proj) then (get-projectile-x proj)
	for py = (get-projectile-y proj) then (get-projectile-y proj)
	for px-int = (round px) then (round px)
	for py-int = (round py) then (round py)
	for red = (create-color 1 0 0) ; CHANGE THIS TO A USER-DEFINED COLOR
	when (or
	      (> px-int (canvas-width canv)) ; projectile too far to the right
	      (< py-int 0)) ; projectile has landed, since Y is negative
	  do (return canv)
	else
	  do (progn
	       (write-pixel canv py-int px-int red)
	       (format t "~&Attempt #~d executed at pixel (~d, ~d)"
		       attempt px-int py-int))
	finally (return canv)))


(defun draw-projectile-path-v2 (canvas projectile env)
  (loop for attempt from 1 upto 50
	for canv = canvas
	for proj = projectile then (tick env proj)
	for px = (get-projectile-x proj) then (get-projectile-x proj)
	for py = (get-projectile-y proj) then (get-projectile-y proj)
	for px-int = (round px) then (round px)
	for py-int = (min (round py) (1- (canvas-height canv)))
	  then (min (round py) (1- (canvas-height canv)))
	for red = (create-color 1 0 0) ; CHANGE THIS TO A USER-DEFINED COLOR
	when (or
	      (> px-int (canvas-width canv)) ; projectile too far to the right
	      (< py-int 0)) ; projectile has landed, since Y is negative
	  do (return canv)
	else
	  do (progn
	       (write-pixel canv py-int px-int red)
	       (format t "~&Attempt #~d executed at pixel (~d, ~d)"
		       attempt px-int py-int))
	finally (return canv)))

(defun draw-projectile-path-v3 (canvas projectile env)
  (do* ((attempt 1 (incf attempt))
       (canv canvas)
       (proj projectile (tick env proj))
       (px (get-projectile-x proj) (get-projectile-x proj))
       (py (get-projectile-y proj) (get-projectile-y proj))
       (px-int (round px) (round px))
       (py-int (min (round py) (1- (canvas-height canv)))
	       (min (round py) (1- (canvas-height canv))))
       (red (create-color 1 0 0))) ; CHANGE THIS TO A USER-DEFINE COLOR
      ((> attempt 50) canv)
    (cond
      ((> px-int (canvas-width canv)) canv) ; projectile too far to the right
      ((< py-int 0) canv) ; projectile has landed, since Y is negative
      (t (progn
	       (write-pixel canv py-int px-int red)
	       (format t "~&Attempt #~d executed at pixel (~d, ~d)"
		       attempt px-int py-int))))))
	   
      
		

;; this creates a file, so uncomment it to run
;; (with-open-file (stream "p:/scripts/lisp/ray/images/projectile.ppm"
;; 			:direction :output :if-exists :supersede)
;;   (format stream
;; 	  (canvas-to-ppm
;; 	   (draw-projectile-path-v3
;; 	    (create-canvas 20 10)
;; 	    (make-projectile
;; 	     :position (tuples:make-vector 0 1 0)
;; 	     :velocity (tuples:scalar-multiply (tuples:normalize
;; 						(tuples:make-vector 1 1.8 0))
;; 					       18))
;; 	    e1))))
	    




;; (format t (canvas-to-ppm
;; 	   (draw-projectile-path-v3
;; 	    (create-canvas 5 20)
;; 	    (make-projectile

;; 	    :position (tuples:make-vector 0 16 0)
;; 	    :velocity (tuples:normalize
;; 		       (tuples:make-vector 1 3 0)))
;; 	    e1)))



