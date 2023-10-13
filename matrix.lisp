(in-package :com.godel.matrix)

(load "p:/scripts/lisp/ray/tuples.lisp")

(defvar idm
  #2A((1 0 0 0) (0 1 0 0) (0 0 1 0) (0 0 0 1))
  "A 4x4 identity matrix")

(defun matrix-height (matrix)
  (array-dimension matrix 0))

(defun matrix-width (matrix)
  (array-dimension matrix 1))

(defgeneric mmul (a b) (:documentation "This is some documentation"))

(defmethod mmul ((a array) (b vector))
  (mmul a (make-array (list (length b) 1) :displaced-to b)))

(defmethod mmul ((A array) (B array))
  (let ((m (matrix-height A))
	(n (matrix-width A))
	(nb (matrix-height B))
	(L (matrix-width B)))
    (assert (= n nb) ()
	    "The second dimension of A ~A must be equal to the first dimension of B ~A"
	    n nb)
    (let ((C (make-array (list m L) :initial-element 0)))
      (loop for i below m do
	(loop for k below L do
	  (setf (aref C i k)
		(loop for j below n
		      sum (* (aref A i j) (aref B j k))))))
      C)))

(defun transpose-4x4 (matrix)
  "Return the transpose of the 4x4 MATRIX."
  (make-array '(4 4) :initial-contents
	      (loop for m below 4
		    collecting (loop for n below 4
				     collecting (aref matrix n m)))))

(defun det-2x2 (matrix)
  "Return the determinant of a 2x2 MATRIX."
  (let ((a (aref matrix 0 0))
	(b (aref matrix 0 1))
	(c (aref matrix 1 0))
	(d (aref matrix 1 1)))
    (- (* a d) (* b c))))
  
(defun submatrix (matrix row col)
  "Return the submatrix of MATRIX by removing row ROW and column COL."
  (let ((width (matrix-width matrix))
	(height (matrix-height matrix)))
    (make-array (list (1- height) (1- width)) :initial-contents
		(loop for i below width unless (= i row)
		      collect (loop for j below height unless (= j col)
				    collect (aref matrix i j))))))

(defun minor (matrix row col)
  "Return the determinant of the submatrix of MATRIX at ROW and COL."
  (det (submatrix matrix row col)))


(defun cofactor (matrix row col)
  "Return the cofactor of MATRIX at ROW and COL."
  (* (if (oddp (+ row col)) -1 1) (minor matrix row col)))

(defun det (matrix)
  "Return the determinant of the MATRIX. This definition always uses the first row to determine the elements. This definition is recursive since it calls COFACTOR which calls MINOR which calls DET."
  (if (equal '(2 2) (array-dimensions matrix))
   (det-2x2 matrix)
    (loop for col below (matrix-width matrix)
	  summing (* (aref matrix 0 col) (cofactor matrix 0 col)))))

(defun singular-p (matrix)
  "Return T if the matrix is singular, ie not invertible."
  (zerop (det matrix)))

(defun invertible-p (matrix)
  "Return T if the matrix is invertible, ie not singular."
  (not (singular-p matrix)))

(defun inverse (matrix)
  "Return the inverse of MATRIX as an array."
  (unless (singular-p matrix)
    (let ((original-det (det matrix))
	  (width (matrix-width matrix))
	  (height (matrix-width matrix)))
      (make-array (list height width)
		  :initial-contents
		  (loop for row below height
			collecting
			(loop for col below width
			      for c = (cofactor matrix col row)
				then (cofactor matrix col row)
			      collecting (/ c original-det)))))))

(defgeneric matrix-equalp (a b) (:documentation "Return T if MATRIX-A is equal to MATRIX-B, using floating point comparisons.  The function can either take matrices or vectors."))

(defmethod matrix-equalp ((a vector) (b vector))
  (matrix-equalp (make-array (list (length a) 1) :displaced-to a)
		 (make-array (list (length b) 1) :displaced-to b)))

(defmethod matrix-equalp ((matrix-A array) (matrix-B array))
  (let ((height (matrix-height matrix-A))
	(width  (matrix-width matrix-A)))
    (loop for row below height
	  for col below width
	  always (floats-equal-p (aref matrix-a row col)
				 (aref matrix-b row col)))))


						   
      
















  
  


