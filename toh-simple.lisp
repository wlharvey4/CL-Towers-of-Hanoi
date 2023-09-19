(in-package :lolh/toh-simple)

(defparameter *towers* '((1 2 3 4 5) () ()))
(defparameter *moves* 0)

(defun towers (from to spare n)
  "A simple Towers of Hanoi solution involving only two procedures:
- towers : the main recursive Towers of Hanoi algorithm that is complete
within itself, calling no other procedures in order to solve the problem;
- simple-towers: the procedure run by the user.  In this case the user
\"simply\" calls `simple-towers` with a single argument: the number of
disks to place on the first tower.  This procedure then calls `towers`
and the algorithm runs, showing the progression of moves and the number
of moves required to solve the problem."
  (if (= n 1)
      ;; when n=1, move a disk from a tower to a tower
      (progn
	(let ((a
		(case from
		  (1 (pop (first *towers*)))
		  (2 (pop (second *towers*)))
		  (3 (pop (third *towers*))))))
	  (case to
	    (1 (push a (first *towers*)))
	    (2 (push a (second *towers*)))
	    (3 (push a (third *towers*)))))
	(format t "~&~30a : ~3d move~:p" *towers* (incf *moves*)))
      (progn
	;; First, move all but the bottom disk to the spare tower;
	(towers from spare to (1- n))
	;; Then move the bottom disk to the destination tower;
	(towers from to spare 1)
	;; Finally move the stack of disks on the spare tower to the
	;; destination tower.
	(towers spare to from (1- n)))))

(defun simple-towers (n)
  "The user calls this procedure to invoke the simple Towers of Hanoi
algorithm; there is one argument: the number of disks to place on the
first tower."
  (setf *towers*
	;; set n disks on the first tower
	(list
	 (loop for m from 1 to n collect m)
	 ()
	 ()))
  (setf *moves* 0) ; reset the number of moves to zero
  (format t "~&~30a : ~3d move~:p" *towers* *moves*)
  ;; start the simple algorithm
  (towers 1 2 3 n))
