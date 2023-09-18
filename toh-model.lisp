;;; toh-model.lisp -- Model for Towers of Hanoi algorithm
;;; Time-stamp: <2023-09-17 19:08:56 wlh>

;;; Created: 2023-09-17

;;; Commentary:

;;; Code:

(in-package :lolh/toh)

(defvar *towers* '((1 2 3 4 5) () ()))
(defparameter *moves* 0)

(defun reset-towers (tower-number number-of-disks)
  "Procedure to set all the disks on the towers.  The first parameter is
the tower number to place the disks onto, and the second parameter is
the number of disks to place on this tower."
  (let ((stack (loop for n from 1 to number-of-disks collect n)))
    (case tower-number
      (1 (setf *towers* (list stack () ())))
      (2 (setf *towers* (list () stack ())))
      (3 (setf *towers* (list () () stack))))))

(defun check-move (from to)
  "Procedure to run before the `move` procedure to make sure the
move is a valid move.  A disk may only be moved onto an empty tower
or a tower holding a larger disk."
  (let (a b)
    (setq a
	  (case from
	    (1 (first *towers*))
	    (2 (second *towers*))
	    (3 (third *towers*))))
    (setq b (case to
	      (1 (first *towers*))
	      (2 (second *towers*))
	      (3 (third *towers*))))
    (and
     (not (null a))
     (or (null b)
	 (< (first a)
	    (first b))))))

(defun move-disk (from to)
  "The procedure that moves a disk from a tower onto a different
tower.  The `check-move` procedure is run first to make sure the move
is a valid move."
  (when (check-move from to)
    (let ((a
	    (case from
	      (1 (pop (first *towers*)))
	      (2 (pop (second *towers*)))
	      (3 (pop (third *towers*))))))
      (case to
	(1 (push a (first *towers*)))
	(2 (push a (second *towers*)))
	(3 (push a (third *towers*))))
      (incf *moves*))))

;;; End toh-model.lisp
