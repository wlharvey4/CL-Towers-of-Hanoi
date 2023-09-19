;;; toh.asd Towers of Hanoi algorithm
;;; Time-stamp: <2023-09-19 02:15:02 wlh>

;;; Created: 2023-09-17
;;; Commentary
;;  System to run the algorithm Towers of Hanoi in Common Lisp.

(defsystem "toh"
  :description "Towers of Hanoi"
  :version "0.3.0"
  :author "LOLH <lincolnlaw@mac.com>"
  :license "CCO 1.0 Universal"
  :components
  ((:file "toh-package")
   (:file "toh-model" :depends-on ("toh-package"))
   (:file "toh-view" :depends-on ("toh-package" "toh-model"))
   (:file "toh-controller" :depends-on ("toh-package" "toh-model"))
   (:file "toh-simple")))

;;; End toh.asd
