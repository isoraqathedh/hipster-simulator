(defpackage :info.isoraqathedh.hipster-simulator.asdf
  (:use #:cl #:asdf))
(in-package :info.isoraqathedh.hipster-simulator.asdf)

(defsystem hipster-simulator
  :name "Hipster Simulator 2014"
  :author "Isoraķatheð Zorethan <isoraqathedh.zorethan@gmail.com>"
  :maintainer "Isoraķatheð Zorethan <isoraqathedh.zorethan@gmail.com>"
  :version "1.0.0"
  :licence "MIT"
  :description "A cellular automaton model of a town of non-conformists."
  :serial t
  :components ((:file "hipsters")))
