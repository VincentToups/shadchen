;;;; tests.lisp

(in-package #:shadchen)

(fiveam:def-suite shadchen-test-suit :description "Tests for shadchen, a pattern matching library.")

(let ((fiveam:*suite* shadchen-test-suit))
  (test match1 
    "Test the MATCH1 macro, the workhorse of the pattern matcher."
    (is (equal *match-fail* (match1 (list x y) (list 10 11 13) (+ x y))))
    (is (equal '(10 (11 12 13))
	       (match1 (cons x y) (list 10 11 12 13) (list x y))))
    (is (equal '(10 11 12)
	       (match1 (list x (list y z)) (list 10 (list 11 12)) (list x y z))))
    (is (equal '(10 12)
	       (match1 (list x (list 'y z)) (list 10 (list 'y 12)) (list x z))))
    (is (equal 'success (match1 (bq (x)) (list 'x) 'success)))
    (is (equal 'success (match1 (bq (x y z)) (list 'x 'y 'z) 'success)))
    (is (equal '(10 11)
	       (match1 (bq (x (uq (list a b)) z))
		       (list 'x (list 10 11) 'z)
		       (list a b))))
    (is (equal '((10 11) 10 11)
	       (match1 (and x (list a b)) (list 10 11) (list x a b))))
    (is (equal 10 (match1 (32 (function numberp) x) 10 x)))
    (is (equal *match-fail* (match1 (32 (function numberp) x) "cat" x)))
    (is (equal 21 (match1 (values x y) (values 10 11) (+ x y))))
    (is (equal 10 (match1 (funcall (function car) x) (cons 10 11) x)))
    (is (equal '(10 (11 12))
	       (match1 (list-rest x y) (list 10 11 12) (list x y))))
    (signals 'simple-error
      (match1 (?) "" ""))
    (is (equal 10 (match1 (let (x 10)) 'any-val x)))
    (is (equal "x"
	       (match1 (or (32 (function stringp) x) (32 (function numberp) x))
		       "x"
		       x)))
    (is (equal 10
	       (match1 (or (32 (function stringp) x) (32 (function numberp) x))
		       10
		       x)))
    (is (equal 'simple-error
	       (match1 (? a b c) "" ""))))

  (test match
    "Testing for the MATCH macro in particular."
  (is (equal
   5 (match (list 1 2 3)
	   ((list x y) (+ x y))
	   ((list x y z) (+ y z)))))
  (signals 'simple-error
    (match (list 1 2 3)
      ((list x) x)
      ((list a b) (+ a b))))
  (is (equal 'string
	 (match "test"
	   ((? #'numberp) 'number)
	   ((? #'stringp) 'string))))))

  



