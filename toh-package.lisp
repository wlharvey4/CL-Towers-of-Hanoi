;;; toh-package.lisp -- Towers of Hanoi Package File
;;; Time-stamp: <2023-09-19 01:49:34 wlh>

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
   :simple-towers))

;; End toh-class.lisp
