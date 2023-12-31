;;; toh-view.lisp -- Towers of Hanoi View Controller file
;;; Time-stamp: <2023-09-19 02:13:34 wlh>

;;; Created: 2023-09-17

;;; Commentary:
;;  These procedures produce the output.

;;; Code:

(in-package :lolh/toh)

(defun towers ()
  *towers*)

(defun view-towers ()
  (format t "~&~a~%" *towers*))

(defun view-towers-and-moves ()
  (format t "~&~30a : ~3d move~:p~%" *towers* *moves*))

(defun move-error ()
  (format t "Move error; try again."))

;;; End toh-view.lisp
