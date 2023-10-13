(in-package :com.godel.colors)

(load "p:/scripts/lisp/ray/tuples.lisp")

(defun create-color (r g b)
  "Quickly make a color as long a R, G and B are in order"
  (make-vec r g b))

(defvar *RED*   (create-color 1 0 0))
(defvar *GREEN* (create-color 0 1 0))
(defvar *BLUE*  (create-color 0 0 1))
(defvar *BLACK* (create-color 0 0 0))
(defvar *WHITE* (create-color 1 1 1))

; color accessor functions
(defun c-red   (c) (aref c 0 0))
(defun c-green (c) (aref c 1 0))
(defun c-blue  (c) (aref c 2 0))

(defun colors-equal-p (c1 c2)
  "Return T if C1 and C2 are approximately equal."
  (and
   (floats-equal-p (c-red c1) (c-red c2))
   (floats-equal-p (c-green c1) (c-green c2))
   (floats-equal-p (c-blue c1) (c-blue c2))))

(defun manipulate-colors (c1 c2 op)
  "A higher-order function to create arithmetic functions on colors."
    (create-color (funcall op (c-red c1) (c-red c2))
	          (funcall op (c-green c1) (c-green c2))
	          (funcall op (c-blue c1) (c-blue c2))))

(defun add-colors (c1 c2)
  "Add two colors"
  (manipulate-colors c1 c2 #'+))

(defun subtract-colors (c1 c2)
  "Subtract two colors."
  (manipulate-colors c1 c2 #'-))

(defun scalar-color (c scalar)
  "Multiply a color C by a SCALAR value."
  (create-color (* (c-red c) scalar)
	        (* (c-green c) scalar)
	        (* (c-blue c) scalar)))

(defun multiply-colors (c1 c2)
  "Return the Hadamard product (or Schur product) of C1 and C2."
  (manipulate-colors c1 c2 #'*))

(defun color-black-p (c)
  "Return T if the color is black."
  (colors-equal-p c *BLACK*))

(defun color-red-p (c)
  "Return T if the color is red."
  (colors-equal-p c *RED*))

(defun color-255-rescale (n)
  "Given N, return a number between 0 and 255."
  (let ((n-rescale (ceiling (* n 255))))
    (cond
      ((> n-rescale 255) 255)
      ((< n-rescale 0) 0)
      (t n-rescale))))

(defun color-to-255-str (c)
  "Return a string that rescales a color to a range between 0 and 255."
  (format nil "~d ~d ~d"
	  (color-255-rescale (c-red c))
	  (color-255-rescale (c-green c))
	  (color-255-rescale (c-blue c))))

(defun color-str-to-list (c-str)
  "Return a list of three elements of C-STR representing the three colors."
  (split-sequence #\Space c-str))
	  















