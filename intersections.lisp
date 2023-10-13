(in-package :com.godel.intersections)

(defstruct (scene-intersection  (:conc-name sint-)
			 (:constructor make-intersection (t-val obj)))
  t-val obj)

(defun intersections (&rest intersects)
  "Return a sorted list of the scene-intersection objects by increasing value of t-values.  Objects with negative t-values are included."
  (sort intersects #'< :key #'sint-t-val))


;; OLD DEFINITION, IF NO ISSUES THIS CAN BE REMOVED
;; (defun intersections (si-1 si-2 &rest other-intersects)
;;   "Return a sorted list of the scene-intersection objects by increasing value of t-values.  Objects with negative t-values are included."
;;   (sort 
;;    (apply #'list si-1 si-2 other-intersects)
;;    #'< :key #'(lambda (el) (sint-t-val el))))

(defun hit (list-of-intersections)
  "Take a collection of intersection records in list form, remove all the records with negative t-values, and return the first one.  This first positive record corresponds to the first 'hit' of the ray."
  (first
   (remove-if-not #'(lambda (an-intersection) (plusp (sint-t-val an-intersection)))
		  list-of-intersections)))
      








  
