;;; toh-controller.lisp -- Towers of Hanoi controller procededures
;;; Time-stamp: <2023-09-17 23:29:56 wlh>

;;; Created: 2023-09-17

;;; Commentary:
;;  These procedures start algorithm or make a move.  The player can
;;  play the game manually using `manual-move-disk.`
;;  `move-tower` is the fundamental recursive algorithm that solves
;;  the problem without needing to know how to solve the problem.
;;  `towers-of-hanoi` starts the program running.

;;; Code:

(in-package :lolh/toh)

(defun manual-move-disk (from to)
  "The user uses this procedure to play the game manually."
  (unless (move-disk from to)
    (move-error))
  (towers))


(defun move-tower (from to spare height)
  "The recursive procedure that moves the disks from a tower to
another tower using a spare tower.  This is the main algorithm."
  (when (plusp height)
    (if (= height 1)
	(if (move-disk from to)
	    (view-towers-and-moves)
	    (move-error))
	(progn
	  (move-tower from spare to (1- height))
	  (move-tower from to spare 1)
	  (move-tower spare to from (1- height))))))

(defun towers-of-hanoi (from-post to-post)
  "The main procedure (along with for `reset-towers`) that should be
exported to be run by the end user.  The first parameter is the tower
that holds the stack of disks, and the second parameter is the tower to
move the stack."
  (reset-moves)
  (let ((height (stack-height from-post))
	(using-spare (calculate-spare from-post to-post)))
    (view-towers)
    (move-tower from-post to-post using-spare height)))


;;; End toh-controller.lisp
