(in-package :com.godel.colors)

(progn
  (load "p:/scripts/lisp/ray/rt.lisp")
  (load "p:/scripts/lisp/ray/colors.lisp"))

(deftest col-1
    (let ((c (create-color -0.5 0.4 1.7)))
      (and
       (= -0.5 (c-red c))
       (=  0.4 (c-green c))
       (=  1.7 (c-blue c)))) t)

(deftest col-2
    (let ((c1 (create-color 0.9 0.6 0.75))
	  (c2 (create-color 0.7 0.1 0.25)))
      (colors-equal-p (create-color 1.6 0.7 1.0)
		      (add-colors c1 c2))) t)


(deftest col-3
(let ((c1 (create-color 0.9 0.6 0.75))
      (c2 (create-color 0.7 0.1 0.25)))
  (colors-equal-p (create-color 0.2 0.5 0.5)
		  (subtract-colors c1 c2))) t)

(deftest col-4
(let ((c (create-color 0.2 0.3 0.4)))
  (colors-equal-p (create-color 0.4 0.6 0.8)
		  (scalar-color c 2))) t)

(deftest col-5
(let ((c1 (create-color 1 0.2 0.4))
      (c2 (create-color 0.9 1 0.1)))
  (colors-equal-p (create-color 0.9 0.2 0.04)
		  (multiply-colors c1 c2))) t)

(deftest col-6
  (let ((c1 (create-color 1.5 0 0))
      (c2 (create-color 0 0.5 0))
      (c3 (create-color -0.5 0 1)))
  (and
   (string-equal "255 0 0" (color-to-255-str c1))
   (string-equal "0 128 0" (color-to-255-str c2))
   (string-equal "0 0 255" (color-to-255-str c3)))) t)

(deftest col-7
  (let ((c1 (color-str-to-list (color-to-255-str (create-color 1.5 0 0))))
      (c2 (color-str-to-list (color-to-255-str (create-color 0 0.5 0))))
      (c3 (color-str-to-list (color-to-255-str (create-color -0.5 0 1)))))
  (and
   (equal c1 '("255" "0" "0"))
   (equal c2 '("0" "128" "0"))
   (equal c3 '("0" "0" "255")))) t)
