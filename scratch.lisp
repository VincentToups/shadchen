;;;; shadchen.lisp

;;; Copyright 2012, Vincent Toups
;;; This program is distributed under the terms of the GNU Lesser 
;;; General Public License (see license.txt).

(in-package #:shadchen)


(defun non-nil (x) x)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defpattern cons* (car cdr)
	`(? #'non-nil (cons ,car ,cdr))))

(match (cons 10 11)
 ((cons* a b) a))

