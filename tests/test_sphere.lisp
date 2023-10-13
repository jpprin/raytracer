(in-package :com.godel.spheres)

(progn
  (load "p:/scripts/lisp/ray/rt.lisp")
  (load "p:/scripts/lisp/ray/matrix.lisp")
  (load "p:/scripts/lisp/ray/transform.lisp")
  (load "p:/scripts/lisp/ray/spheres.lisp"))

(deftest sphere-default-transform
    (let ((s (make-sphere)))
      (matrix-equalp (sphere-transform s) idm)) t)

(deftest change-sphere-transformation
    (let ((s (make-sphere))
	  (tr (transform:translation 2 3 4)))
      (progn
	(set-transform s tr)
	(matrix-equalp (sphere-transform s) tr))) t)

(deftest intersect-scaled-sphere-with-ray
    (let* ((r (make-ray (make-point 0 0 -5) (make-vec 0 0 1)))
	   (s (make-sphere)))
      (progn
	(set-transform s (scaling 2 2 2))
	(let ((xs (intersect s r)))
	  (and
	   (= 2 (length xs))
	   (= 3 (sint-t-val (first xs)))
	   (= 7 (sint-t-val (second xs))))))) t)

(deftest intersect-translated-sphere-with-ray
    (let* ((r (make-ray (make-point 0 0 -5) (make-vec 0 0 1)))
	   (s (make-sphere)))
      (progn
	(set-transform s (translation 5 0 0))
	(let ((xs (intersect s r)))
	  (zerop (length xs))))) t)

(deftest normal-sphere-x-axis
  (let* ((s (make-sphere))
	 (n (normal-at s (make-point 1 0 0))))
    (equalp n (make-vec 1 0 0))) t)

(deftest normal-sphere-y-axis
  (let* ((s (make-sphere))
	 (n (normal-at s (make-point 0 1 0))))
    (equalp n (make-vec 0 1 0))) t)

(deftest normal-sphere-z-axis
  (let* ((s (make-sphere))
	 (n (normal-at s (make-point 0 0 1))))
    (equalp n (make-vec 0 0 1))) t)

(deftest normal-sphere-nonaxial
    (let* ((s (make-sphere))
	   (sqrt-3-over-3 (/ (sqrt 3) 3))
	   (n (normal-at s (make-point sqrt-3-over-3 sqrt-3-over-3 sqrt-3-over-3))))
      (matrix-equalp n (make-vec sqrt-3-over-3 sqrt-3-over-3 sqrt-3-over-3))) t)


(deftest normal-is-normalized-vector
      (let* ((s (make-sphere))
	   (sqrt-3-over-3 (/ (sqrt 3) 3))
	     (n (normal-at s (make-point sqrt-3-over-3 sqrt-3-over-3 sqrt-3-over-3))))
	(matrix-equalp n (normalize n))) t)

(deftest normal-on-translated-sphere
  (let ((s (make-sphere)))
    (set-transform s (translation 0 1 0))
    (let ((n (normal-at s (make-point 0 1.70711 -0.70711))))
      (matrix-equalp n (make-vec 0 0.70711 -0.70711)))) t)

(deftest normal-on-transformed-sphere
  (let ((s (make-sphere))
	(m (mmul (scaling 1 0.5 1) (rotation-z (/ pi 5)))))
    (set-transform s m)
    (let ((n (normal-at s (make-point 0 (/ (sqrt 2) 2) (- (/ (sqrt 2) 2))))))
      (matrix-equalp n (make-vec 0 0.97014 -0.24254)))) t)

(deftest sphere-has-material
  (let* ((s (make-sphere))
	 (m (sphere-material s)))
    (equalp m (make-material))) t)
	
(deftest sphere-assigned-material
  (let ((s (make-sphere))
	(m (make-material)))
    (setf (material-ambient m) 1)
    (setf (sphere-material s) m)
    (equal (sphere-material s) m)) t)
	

; The tests below will no longer work, since after they were written, the definition of
; INTERSECT was changed to make the function return a SCENE-INTERSECTION object
; rather than the real number corresponding to t-values


;; (deftest ray-sphere-intersect
;;     (let* ((r (make-ray (make-point 0 0 -5) (make-vec 0 0 1)))
;; 	   (s (make-sphere))
;; 	   (xs (intersect s r)))
;;       (and
;;        (= (length xs) 2)
;;        (= (first xs) 4.0)
;;        (= (second xs) 6.0))) t)

;; (deftest tangent-intersect
;;   (let* ((r (make-ray (make-point 0 1 -5) (make-vec 0 0 1)))
;; 	(s (make-sphere))
;; 	(xs (intersect s r)))
;;     (and
;;      (= (length xs) 2)
;;      (= (first xs) 5.0)
;;      (= (second xs) 5.0))) t)

;; (deftest ray-misses-sphere
;;   (let* ((r (make-ray (make-point 0 2 -5) (make-vec 0 0 1)))
;; 	(s (make-sphere))
;; 	(xs (intersect s r)))
;;     (zerop (length xs))) t)

;; (deftest ray-inside-sphere
;;   (let* ((r (make-ray (make-point 0 0 0) (make-vec 0 0 1)))
;; 	(s (make-sphere))
;; 	(xs (intersect s r)))
;;     (and
;;      (= (length xs) 2)
;;      (= (first xs) -1.0)
;;      (= (second xs) 1))) t)

;; (deftest ray-behind-sphere
;;   (let* ((r (make-ray (make-point 0 0 5) (make-vec 0 0 1)))
;; 	 (s (make-sphere))
;; 	 (xs (intersect s r)))
;;     (and
;;      (= (length xs) 2)
;;      (= (first xs) -6.0)
;;      (= (second xs) -4.0))) t)

