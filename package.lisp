;;;; package.lisp

(defpackage #:shadchen
;;;  (:export "MATCH" "MATCH-LAMBDA" "DEFPATTERN")
  (:export :match :match-lambda :defpattern
		   :? :bq :list-rest :hash-table :struct :let1)
  (:use #:cl))


