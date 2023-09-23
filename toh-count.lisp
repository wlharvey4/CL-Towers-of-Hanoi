;;; toh-count.lisp --- Towers of Hanoi Simple Counting Procedure
;;; Time-stamp: <2023-09-22 22:21:44 wlh>

;;; Created 2023-09-22

;;;Commentary: Problem 5.5 in Winston-Horn Lisp 3rd Edition

;;; Code:

(in-package :lolh/toh-simple)

(defun toh-count (l)
  (if (endp l)
      0
      (+ (toh-count (rest l))
	 1
	 (toh-count (rest l)))))

;;; End toh-count.lisp
