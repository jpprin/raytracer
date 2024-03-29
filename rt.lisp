(defvar *test* nil "Current test name")
(defvar *do-tests-when-defined* nil)
(defvar *entries* '(nil) "Test database")
(defvar *in-test* nil "Used by TEST")
(defvar *debug* nil "For debugging")

(defstruct (entry (:conc-name nil)
		  (:type list))
  pend name form)

(defmacro vals (entry) `(cdddr ,entry))

(defmacro defn (entry) `(cdr ,entry))

(defun pending-tests ()
  (do ((l (cdr *entries*) (cdr l))
       (r nil))
      ((null l) (nreverse r))
    (when (pend (car l))
      (push (name (car l)) r))))

(defun rem-all-tests ()
  (setq *entries* (list nil))
  nil)

(defun rem-test (&optional (name *test*))
  (do ((l *entries* (cdr l)))
      ((null (cdr l)) nil)
    (when (equal (name (cadr l)) name)
      (setf (cdr l) (cddr l))
      (return name))))

(defun get-test (&optional (name *test*))
  (defn (get-entry name)))

(defun get-entry (name)
  (let ((entry (find name (cdr *entries*)
		     :key #'name
		     :test #'equal)))
    (when (null entry)
      (report-error t
		    "~%No test with name: ~:@(~S~)."
		    name))
    entry))

(defmacro deftest (name form &rest values)
  `(add-entry '(t ,name ,form .,values)))

(defun add-entry (entry)
  (setq entry (copy-list entry))
  (do ((l *entries* (cdr l))) (nil)
    (when (null (cdr l))
      (setf (cdr l) (list entry))
      (return nil))
    (when (equal (name (cadr l))
		 (name entry))
      (setf (cadr l) entry)
      (report-error nil
		    "Redefining tests ~@:(~S~)"
		    (name entry))
      (return nil)))
  (when *do-tests-when-defined*
    (do-entry entry))
  (setq *test* (name entry)))

(defun report-error (error? &rest args)
  (cond (*debug*
	 (apply #'format t args)
	 (if error? (throw '*debug* nil)))
	(error? (apply #'error args))
	(t (apply #'warn args))))

; CODE TO ACTUALLY PERFORM TESTS

(defun do-test (&optional (name *test*))
  (do-entry (get-entry name)))

(defun do-entry (entry &optional (s *standard-output*))
  (catch '*in-test*
    (setq *test* (name entry))
    (setf (pend entry) t)
    (let* ((*in-test* t)
	   (*break-on-warnings* t)
	   (r (multiple-value-list
	       (eval (form entry)))))
      (setf (pend entry)
	    (not (equal r (vals entry))))
      (when (pend entry)
	(format s "~&Test ~:@(~S~) failed~
                   ~%Form: ~S~
                   ~%Expected value ~P: ~
                      ~{~S~^~%~17t~}~
                   ~%Actual value~P: ~
                      ~{~S~^~%15t~}.~%"
		*test* (form entry)
		(length (vals entry))
		(vals entry)
		(length r) r))))
  (when (not (pend entry)) *test*))

(defun continue-testing ()
  (if *in-test*
      (throw '*in-test* nil)
      (do-entries *standard-output*)))

(defun do-tests (&optional (out *standard-output*))
  (dolist (entry (cdr *entries*))
    (setf (pend entry) t))
  (if (streamp out)
      (do-entries out)
      (with-open-file
	  (stream out :direction :output)
	(do-entries stream))))

(defun do-entries (s)
  (format s "~&Doing ~A pending test~:P ~
             of ~A tests total.~%"
	  (count t (cdr *entries*)
		 :key #'pend)
	  (length (cdr *entries*)))
  (dolist (entry (cdr *entries*))
    (when (pend entry)
      (format s "~@[~<~%~:; ~:@(~S~)~>~]"
	      (do-entry entry s))))
  (let ((pending (pending-tests)))
    (if (null pending)
	(format s "~&No tests failed.")
	(format s "~%~A out of ~A ~
                   total tests failed: ~
                   ~:@(~{~<~%   ~1:;~S~>~
                         ~^, ~}~)."
		(length pending)
		(length (cdr *entries*))
		pending))
    (null pending)))

