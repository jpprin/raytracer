(in-package :com.godel.materials)

(progn
  (load "P:/scripts/lisp/ray/colors.lisp")
  (load "P:/scripts/lisp/ray/lights.lisp")
  )

(defstruct material
  (color *WHITE*) (ambient 0.1) (diffuse 0.9) (specular 0.9) (shininess 200.0))

(defun lighting (material light point eyev normalv)
  "Returns a color by using the Phong reflection model to shade objects so they appear three-dimensional.  Adds together the material's ambient, diffuse, and specular components, weighting by the angles between the vectors."
  (let*
      (
       ; combine the surface color with the light's color/intensity
       (effective-color (multiply-colors
			 (material-color material)
			 (lights:light-intensity light)))
       ; find the direction to the light source
       (lightv (normalize (subtract-tuples
			   (lights:light-position light) point)))
       ; compute the ambient contribution
       (ambient (scalar-color effective-color (material-ambient material)))
       (light-dot-normal (dot lightv normalv))
       (diffuse)
       (specular)
       (factor)
       (reflect-dot-i)
       (reflectv)
       )
    (if (minusp light-dot-normal)

	  (setf diffuse *BLACK*
		specular *BLACK*)


	  ; else compute the diffuse contribution
	(progn
	  (setf diffuse (scalar-color effective-color
				      (* (material-diffuse material)
					 light-dot-normal))
		reflectv (reflect (negate-tuple lightv) normalv)
		reflect-dot-i (dot reflectv eyev))
	  (if (<= reflect-dot-i 0)
	      (setf specular *BLACK*)
	      ; else

		(setf factor (expt reflect-dot-i (material-shininess material))
		      specular (scalar-color (lights:light-intensity light)
				  (* (material-specular material) factor))))))
    (add-colors ambient (add-colors diffuse specular))))


  

	 
