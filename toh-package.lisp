;;; toh-package.lisp -- Towers of Hanoi Package File
;;; Time-stamp: <2023-09-22 22:15:54 wlh>

;;; Created: 2023-09-17

;;; Commentary:
;;  Package definition file for Towers of Hanoi algorithm in Common Lisp.

;;; Code:

(defpackage :lolh/toh
  (:use :cl)
  (:export
   :*towers*
   :view-towers
   :view-towers-and-moves
   :reset-towers
   :manual-move-disk
   :towers-of-hanoi))

(defpackage :lolh/toh-simple
  (:use :cl)
  (:export
   :simple-towers
   :toh-count))

;; End toh-class.lisp
