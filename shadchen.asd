;;;; shadchen.asd
;;; Copyright 2012, Vincent Toups
;;; This program is distributed under the terms of the GNU Lesser 
;;; General Public License (see license.txt).

(asdf:defsystem #:shadchen
	:serial t
	:author "Vincent Toups"
	:maintainer "Vincent Toups"
	:description "A pattern matching library."
	:long-description "Shadchen (matchmaker) is a Racket-inspired pattern matching library."
	:components ((:file "package")
				 (:file "shadchen")))

