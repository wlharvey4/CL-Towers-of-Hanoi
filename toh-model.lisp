;;; toh-model.lisp -- Model for Towers of Hanoi algorithm
;;; Time-stamp: <2023-09-17 23:27:18 wlh>

;;; Created: 2023-09-17

;;; Commentary:
;;  *towers* is a list holding three lists representing the three
;;  towers.  Numbers represent the different disks sitting on the
;;  towers; the larger the number, the larger the size of the disk.
;;  Disks are moved from one tower to another one at a time; smaller
;;  disks may be placed on larger disks, but not vice versa.  The
;;  objective of the player is to move all of the disks from one tower
;;  to another.
;;  *moves* records the number of moves it takes to move a tower of
;;  disks from one disk to another.
;;  `reset-towers` allows the player to reset the towers so that all
;;  disks sit on one tower.  The player can choose which tower to
;;  place the disks, and how many disks to place there (the initial
;;  numbrer of disks is five).
;;  `move-disk` is the procedure that moves a disk from one tower to
;;  another, first checking that the move is legal.

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

(defun reset-moves ()
  (setf *moves* 0))

(defun stack-height (tower)
  "Returns the number of disks sitting on a given tower."
  (length
   (case tower
     (1 (first *towers*))
     (2 (second *towers*))
     (3 (third *towers*)))))

(defun calculate-spare (from to)
  (case (+ from to)
    (3 3)
    (4 2)
    (5 1)))

(defun check-move (from to)
  "Procedure to run before the `move` procedure to make sure the
move is a valid move.  A disk may only be moved onto an empty tower
or a tower holding a larger disk."
  (let ((a (case from
	     (1 (first *towers*))
	     (2 (second *towers*))
	     (3 (third *towers*))))
	(b (case to
	      (1 (first *towers*))
	      (2 (second *towers*))
	      (3 (third *towers*)))))
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
    (let ((a (case from
	       (1 (pop (first *towers*)))
	       (2 (pop (second *towers*)))
	       (3 (pop (third *towers*))))))
      (case to
	(1 (push a (first *towers*)))
	(2 (push a (second *towers*)))
	(3 (push a (third *towers*))))
      (incf *moves*))))

;;; End toh-model.lisp
