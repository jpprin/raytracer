(in-package :com.godel.world)

(defstruct world light objects)
	   
(defun default-world ()
      (make-world
       :light (point-light (make-point -10 10 -10) *WHITE*)
       :objects
       (list
	(make-sphere :material
			 (make-material
			  :color (create-color 0.8 1.0 0.6)
			  :diffuse 0.7
			  :specular 0.2))
	(make-sphere :transform (scaling 0.5 0.5 0.5)))))


;; ONLY HALFAWY DONE WITH THIS      
;; (defun intersect-world (a-world a-ray)
;;   (loop for each-object in (world-objects a-world)
;; 	collecting (intersect each-object a-ray)))


