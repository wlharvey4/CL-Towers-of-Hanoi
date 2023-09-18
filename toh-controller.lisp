;;; tow-controller.lisp -- Towers of Hanoi controller procededures
;;; Time-stamp: <2023-09-17 22:12:08 wlh>

;;; Created: 2023-09-17

;;; Commentary:

;;; Code:

(in-package :lolh/toh)

(defun manual-move-disk (from to)
  (unless (move-disk from to)
    (move-error))
  (towers))


(defun move-tower (from to spare n)
  "The recursive procedure that moves the disks from a tower to
another tower using a spare tower.  This is the main algorithm."
  (when (plusp n)
    (if (= n 1)
	(if (move-disk from to)
	    (view-towers-and-moves)
	    (move-error))
	(progn
	  (move-tower from spare to (1- n))
	  (move-tower from to spare 1)
	  (move-tower spare to from (1- n))))))

(defun towers-of-hanoi (from-post to-post)
  "The main procedure (along with for `reset-towers`) that should be
exported to be run by the end user.  The first parameter is the post
that holds the stack of disks, and the second parameter is the post to
move the stack."
  (setf *moves* 0)
  (let ((n (length
	    (case from-post
	      (1 (first *towers*))
	      (2 (second *towers*))
	      (3 (third *towers*)))))
	   (using-spare
	    (case (+ from-post to-post)
	      (3 3)
	      (4 2)
	      (5 1))))
	(view-towers)
	(move-tower from-post to-post using-spare n)))


;;; End toh-controller.lisp
