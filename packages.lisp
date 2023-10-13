
(in-package :cl)

(defpackage :com.godel.rt
  (:use :common-lisp)
  (:nicknames :rt)
  (:export :deftest :get-test :do-test :rem-test :rem-all-tests :do-tests
	   :pending-tests :continue-testing :*test* :*do-tests-when-defined*))

(defpackage :com.godel.tuples
  (:use :common-lisp)
  (:nicknames :tuples)
  (:export :tuple-x :tuple-y :tuple-z :floats-equal-p :make-point :point-x
	   :point-y :point-z :make-vec :add-tuples :subtract-tuples
   :negate-tuple :normalize :dot :cross :reflect :scalar-multiply))

(defpackage :com.godel.colors
  (:use :common-lisp :tuples :cl-utilities)
  (:nicknames :colors)
  (:export :create-color :colors-equal-p :add-colors :subtract-colors
	   :scalar-color :multiply-colors :color-black-p :color-red-p
	   :color-to-255-str :color-str-to-list
   :*RED* :*GREEN* :*BLUE* :*BLACK* :*WHITE*))

(defpackage :com.godel.canvas
  (:use :common-lisp :colors)
  (:nicknames :canvas)
  (:export :create-canvas :canvas-width :canvas-height :write-pixel :pixel-at
   :canvas-to-ppm))

(defpackage :com.godel.projectiles
  (:use :common-lisp :com.godel.tuples :com.godel.colors
   :com.godel.canvas))

(defpackage :com.godel.matrix
  (:use :common-lisp :com.godel.tuples)
  (:nicknames :matrix)
  (:export :idm :mmul :transpose-4x4 :det :inverse :matrix-equalp))

(defpackage :com.godel.transform
  (:use :common-lisp :com.godel.tuples :com.godel.matrix)
  (:nicknames :transform)
  (:export :translation :scaling :rotation-x :rotation-y :rotation-z
   :shearing))

(defpackage :com.godel.raytest
  (:use :common-lisp :fiveam :tuples :com.godel.colors
	:com.godel.canvas))

(defpackage :com.godel.clock
  (:use :common-lisp :tuples :com.godel.canvas :com.godel.colors
	:com.godel.matrix :com.godel.transform))

(defpackage :com.godel.rays
  (:use :common-lisp :tuples :matrix :com.godel.transform)
  (:nicknames :rays)
  (:export :ray :make-ray :ray-origin :ray-direction :ray-position :transform))

(defpackage :com.godel.intersections
  (:use :common-lisp :tuples :com.godel.rays)
  (:nicknames :intersections)
  (:export :make-intersection :sint-t-val :hit))

(defpackage :com.godel.materials
  (:use :common-lisp :tuples :colors)
  (:nicknames :materials)
  (:export :make-material :material-color :material-ambient :material-diffuse
	   :material-specular :material-shininess :lighting))

(defpackage :com.godel.spheres
  (:use :common-lisp :tuples :matrix :transform :rays :intersections
	:materials)
  (:nicknames :spheres)
  (:export :make-sphere :sphere-material :intersect :normal-at))

(defpackage :com.godel.silhouette
  (:use :common-lisp :tuples :colors :canvas :matrix :transform :rays
   :intersections :spheres))

(defpackage :com.godel.lights
  (:use :common-lisp :tuples :colors)
  (:nicknames :lights)
  (:export :point-light :light-position :light-intensity))

(defpackage :com.godel.firstrender
  (:use :common-lisp :tuples :colors :canvas :matrix :transform :rays
   :intersections :materials :spheres :lights)
  (:nicknames :firstrender))

(defpackage :com.godel.world
  (:use :common-lisp :cl-user :tuples :colors :transform :rays :materials :spheres :lights)
  (:nicknames :world))
