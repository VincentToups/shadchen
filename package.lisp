;;;; package.lisp

(defpackage #:shadchen
  (:nicknames :s?)
  (:export :? :bq :p :list-rest :hash-table :struct :let1 
		   :must-match :!
		   :match :match-lambda :defpattern
		   :match-let :match-let* :match-loop
		   :tail :number :symbol :string :keyword
		   :_)
  (:use #:cl))




