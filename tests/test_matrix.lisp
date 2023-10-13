(in-package :com.godel.matrix)

(progn
  (load "p:/scripts/lisp/ray/rt.lisp")
  (load "p:/scripts/lisp/ray/matrix.lisp"))

(deftest matrix-test-1 
    (let ((m #2A((1 2 3 4)
		 (5.5 6.5 7.5 8.5)
		 (9 10 11 12)
		 (13.5 14.5 15.5 16.5))))
      (= 1 (aref m 0 0))
      (= 4 (aref m 0 3))
      (= 5.5 (aref m 1 0))
      (= 7.5 (aref m 1 2))
      (= 11 (aref m 2 2))
      (= 13.5 (aref m 3 0))
      (= 15.5 (aref m 3 2)))
  t)

(deftest matrix-test-2
    (let ((m (make-array '(2 2) :initial-contents '((-3 5)
						    (1 -2)))))
      (= -3 (aref m 0 0))
      (= 5 (aref m 0 1))
      (= 1 (aref m 1 0))
      (= -2 (aref m 1 1)))
  t)

(deftest matrix-test-3
    (let ((m (make-array '(3 3) :initial-contents '((-3 5 0)
						    (1 -2 -7)
						    (0 1 1)))))
      (= -3 (aref m 0 0))
      (= -2 (aref m 1 1))
      (= 1 (aref m 2 2)))
  t)

(deftest matrix-test-4
    (let ((a (make-array '(4 4) :initial-contents '((1 2 3 4)
						    (5 6 7 8)
						    (9 8 7 6)
						    (5 4 3 2))))
	  (b (make-array '(4 4) :initial-contents '((1 2 3 4)
						    (5 6 7 8)
						    (9 8 7 6)
						    (5 4 3 2)))))
      (equalp a b))
  t)

(deftest matrix-test-5
    (let ((a (make-array '(4 4) :initial-contents '((1 2 3 4)
						    (5 6 7 8)
						    (9 8 7 6)
						    (5 4 3 2))))
	  (b (make-array '(4 4) :initial-contents '((2 3 4 5)
						    (6 7 8 9)
						    (8 7 6 5)
						    (4 3 2 1)))))
      (not (equalp a b)))
  t)

(deftest matrix-test-6
    (let* ((a #2A((1 2 3 4) (5 6 7 8) (9 8 7 6) (5 4 3 2)))
	   (b #2A((-2 1 2 3) (3 2 1 -1) (4 3 6 5) (1 2 7 8)))
	   (ab-prod (mmul a b)))
      (equalp ab-prod
	      (make-array '(4 4) :initial-contents '((20 22 50 48)
						     (44 54 114 108)
						     (40 58 110 102)
						     (16 26 46 42)))))
  t)

(deftest matrix-test-7
    (let* ((a #2A((1 2 3 4) (2 4 4 2) (8 6 4 1) (0 0 0 1)))
	   (b #2A((1) (2) (3) (1)))
	   (ab-prod (mmul a b)))
      (equalp ab-prod
	      #2A((18) (24) (33) (1))))
  t)

(deftest matrix-test-8
    (let* ((a #2A((0 1 2 4) (1 2 4 8) (2 4 8 16) (4 8 16 32)))
	   (a-prod (mmul a idm))
	   (a-prod-2 (mmul idm a)))
      (and (equalp a a-prod) (equalp a a-prod-2)))
  t)

(deftest matrix-test-9
    (let* ((a #2A((1) (2) (3) (4)))
	   (a-prod (mmul idm a)))
      (equalp a-prod a))
  t)

(deftest matrix-test-10
    (let ((m1 (make-array '(4 4)
			  :initial-contents '((0 9 3 0)
					      (9 8 0 8)
					      (1 8 5 3)
					      (0 0 5 8))))
	  (m2 (make-array '(4 4)
			  :initial-contents '((0 9 1 0)
					      (9 8 8 0)
					      (3 0 5 5)
					      (0 8 3 8)))))
      (equalp (transpose-4x4 m1) m2))
  t)

(deftest matrix-test-11 (equalp idm (transpose-4x4 idm)) t)

(deftest matrix-test-12
    (let ((m (make-array '(2 2) :initial-contents '((1 5) (-3 2)))))
      (= 17 (det-2x2 m)))
  t)

(deftest matrix-test-13
    (let ((a    (make-array '(3 3) :initial-contents '((1 5 0)
						       (-3 2 7)
						       (0 6 -3))))
	  (suba (make-array '(2 2) :initial-contents '((-3 2)
						       (0 6)))))
      (equalp (submatrix a 0 2) suba))
  t)

(deftest matrix-test-14
    (let ((a    (make-array '(4 4) :initial-contents '((-6 1 1 6)
						       (-8 5 8 6)
						       (-1 0 8 2)
						       (-7 1 -1 1))))
	  (suba (make-array '(3 3) :initial-contents '((-6 1 6)
						       (-8 8 6)
						       (-7 -1 1)))))
      (equalp (submatrix a 2 1) suba))
  t)

(deftest matrix-test-15
    (let* ((a (make-array '(3 3) :initial-contents '((3 5 0)
						     (2 -1 -7)
						     (6 -1 5))))
	   (b (submatrix a 1 0)))
      (and (= 25 (det-2x2 b))
	   (= 25 (minor a 1 0))))
  t)

(deftest matrix-test-16
    (let ((a (make-array '(3 3) :initial-contents '((3 5 0)
						    (2 -1 -7)
						    (6 -1 5)))))
      (and
       (= -12 (minor a 0 0))
       (= -12 (cofactor a 0 0))
       (= 25 (minor a 1 0))
       (= -25 (cofactor a 1 0))))
  t)

(deftest matrix-test-17
    (let ((a (make-array '(3 3) :initial-contents '((1 2 6)
						    (-5 8 -4)
						    (2 6 4)))))
      (and
       (= 56 (cofactor a 0 0))
       (= 12 (cofactor a 0 1))
       (= -46 (cofactor a 0 2))
       (= -196 (det a))))
  t)

(deftest matrix-test-18
    (let ((a (make-array '(4 4) :initial-contents '((-2 -8 3 5)
						    (-3 1 7 3)
						    (1 2 -9 6)
						    (-6 7 7 -9)))))
      (and
       (= 690 (cofactor a 0 0))
       (= 447 (cofactor a 0 1))
       (= 210 (cofactor a 0 2))
       (= 51 (cofactor a 0 3))
       (= -4071 (det a))))
  t)

(deftest matrix-test-19
    (let ((a (make-array '(4 4) :initial-contents '((6 4 4 4)
						    (5 5 7 6)
						    (4 -9 3 -7)
						    (9 1 7 -6)))))
      (and
       (= -2120 (det a))
       (invertible-p a)))
  t)

(deftest matrix-test-20
    (let ((a (make-array '(4 4) :initial-contents '((-4 2 -2 -3)
						    (9 6 2 6)
						    (0 -5 1 -5)
						    (0 0 0 0)))))
      (and
       (zerop (det a))
       (null (invertible-p a))))
  t)


(deftest matrix-test-21
    (let* ((a (make-array '(4 4) :initial-contents '((-5 2 6 -8)
						     (1 -5 1 8)
						     (7 7 -6 -7)
						     (1 -3 7 4))))
	   (b (inverse a)))
      (and
       (= (det a) 532)
       (= (cofactor a 2 3) -160)
       (= (aref b 3 2) (/ -160 532))
       (= (cofactor a 3 2) 105)
       (= (aref b 2 3) (/ 105 532))
       (matrix-equalp b
		      (make-array '(4 4) :initial-contents
				  '((0.21805 0.45113 0.24060 -0.04511)
				    (-0.80827 -1.45677 -0.44361 0.52068)
				    (-0.07895 -0.22368 -0.05263 0.19737)
				    (-0.52256 -0.81391 -0.30075 0.30639))))))
  t)

(deftest matrix-test-22
    (let* ((a (make-array '(4 4) :initial-contents '((8 -5 9 2)
						     (7 5 6 1)
						     (-6 0 9 6)
						     (-3 0 -9 -4))))
	   (b (inverse a)))
      (matrix-equalp b
		     (make-array '(4 4) :initial-contents
				 '((-0.15385 -0.15385 -0.28205 -0.53846)
				   (-0.07692  0.12308  0.02564  0.03077)
				   ( 0.35897  0.35897  0.43590  0.92308)
				   (-0.69231 -0.69231 -0.76923 -1.92308)))))
  t)

(deftest matrix-test-23
    (let* ((a (make-array '(4 4) :initial-contents '((9 3 0 9) (-5 -2 -6 -3) (-4 9 6 4)
						     (-7 6 6 2))))
	   (b (inverse a)))
      (matrix-equalp b
		     (make-array '(4 4) :initial-contents
				 '((-0.04074 -0.07778  0.14444 -0.22222)
				   (-0.07778  0.03333  0.36667 -0.33333)
				   (-0.02901 -0.14630 -0.10926  0.12963)
				   ( 0.17778  0.06667 -0.26667  0.33333)))))
  t)

(deftest matrix-test-24
    (let* ((a (make-array '(4 4) :initial-contents '((3 -9 7 3)
						     (3 -8 2 -9)
						     (-4 4 4 1)
						     (-6 5 -1 1))))
	   (b (make-array '(4 4) :initial-contents '((8 2 2 2)
						     (3 -1 7 0)
						     (7 0 5 4)
						     (6 -2 0 5))))
	   (c (mmul a b)))
      (matrix-equalp (mmul c (inverse b)) a))
  t)
