;;;; package.lisp

(defpackage #:shadchen
  (:nicknames :s?)
  (:export :? :bq :p :list-rest :hash-table :struct :let1 
		   :match :match-lambda :defpattern
		   :match-let :match-let* :match-loop
		   :_)
  (:use #:cl))




