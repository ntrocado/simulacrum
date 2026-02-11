;;;; simulacrum.asd

(asdf:defsystem #:simulacrum
  :description "Describe simulacrum here"
  :author "Nuno Trocado"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (#:cl-collider #:coremidi)
  :components ((:file "package")
	       (:file "setup")
               (:file "simulacrum")))
