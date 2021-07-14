;;;; htmaker.asd

(asdf:defsystem #:htmaker
  :description "Describe htmaker here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (:iterate 
  	:trivial-shell 
	:split-sequence 
	:markdown.cl 
	:external-program
	:bordeaux-threads)
  :components ((:file "package")
			   (:file "utils")
			   (:file "source-file-parser")
			   (:file "parser")
			   (:file "config")
               (:file "htmaker")
			   ))

