;;;; tests.lisp

(in-package #:shadchen)

(eos:test list1 
  (eos:is (equal (list :q :r :s)
				 (match (list :s :r :q)
				   ((list x y z)
					(list z y x))))))
(eos:test list2
  (eos:is (equal 
		   (* 1 2 3)
		   (match (list 1 2 3)
			 ((list x y z)
			  (* x y z))))))

(eos:test list-tail 
  (eos:is (equal '(x y z)
				 (match '(q r s t u v w x y z)
				   ((list a b c d e f g (tail the-tail))
					the-tail)))))

(eos:test numeric-literal
  (eos:is (equal 
		   :matched (match 10
					  (11 :did-not-match)
					  (10 :matched)))))

(eos:test string-literal
  (eos:is (equal 
		   :matched (match "cat"
					  (15 :did-not-match)
					  ("cat" :matched)))))

(eos:test keyword-literal
  (eos:is (equal 
		   :matched
		   (match :x
			 (15 :did-not-match)
			 (:x :matched)))))

(eos:test cons
  (eos:is 
   (equal '(b c)
		  (match '(a b c)
			(15 :did-not-match)
			((cons a tail)
			 tail)))))

(eos:test quote
  (eos:is 
   (equal :matched
		  (match 'x
			((cons a b)
			 :did-not-match)
			('x :matched)))))

(eos:test number1
  (eos:is 
   (equal :matched 
		  (match 10
			((symbol) :did-not-match)
			((number) :matched)))))


(eos:run!)
