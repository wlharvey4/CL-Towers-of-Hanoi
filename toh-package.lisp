;;; toh-package.lisp -- Towers of Hanoi Package File
;;; Time-stamp: <2023-09-17 21:18:04 wlh>

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
   :towers-of-hanoi))

;; End toh-class.lisp
