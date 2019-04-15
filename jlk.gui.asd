;;; -*- mode: Common-Lisp; coding: utf-8-unix -*-

(asdf:defsystem :jlk.gui
  :description ""
  :version "0.1"
  :author "JLK"
  :license "BSD 3-Clause"
  :serial t
  :depends-on (:iterate
	       :optima
               :optima.ppcre
	       :cl-annot)
  :components ((:file "src/package")
	       (:file "src/colour")
	       (:file "src/colour-palette-solarized")
	       (:file "src/config")))
