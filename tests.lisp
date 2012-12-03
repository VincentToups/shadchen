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
				   ((list _ _ _ _ _ _ _ (tail the-tail))
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
			((cons -ignore- tail)
			 tail)))))

(eos:test quote
  (eos:is 
   (equal :matched
		  (match 'x
			((cons _ _)
			 :did-not-match)
			('x :matched)))))

(eos:test number1
  (eos:is 
   (equal :matched 
		  (match 10
			((symbol) :did-not-match)
			((number) :matched)))))

(eos:test number2
  (eos:is
   (equal :matched
		  (match 10
			((number (p #'(lambda (x)
							(< x 9))))
			 :did-not-match)
			((number (p #'(lambda (x)
							(> x 9))))
			 :matched)))))

(eos:test string1
  (eos:is 
   (equal :matched 
		  (match "x"
			((symbol) :did-not-match)
			((string) :matched)))))

(eos:test string2
  (eos:is
   (equal :matched
		  (match "cat"
			((string (p #'(lambda (x)
							(= 7 (length x)))))
			 :did-not-match)
			((string (p #'(lambda (x)
							(= (length x) 3))))
			 :matched)))))

(eos:test keyword1
  (eos:is 
   (equal :matched 
		  (match :x
			((non-kw-symbol) :did-not-match)
			((keyword) :matched)))))

(eos:test keyword2
  (eos:is
   (equal :matched
		  (match :cat
			((keyword (p #'(lambda (x)
							 (= 7 (length (symbol-name x))))))
			 :did-not-match)
			((keyword (p #'(lambda (x)
							 (= (length (symbol-name x)) 3))))
			 :matched)))))

(eos:test p
  (eos:is 
   (equal 10
		  (match 10
			((p #'numberp n)
			 n)))))

(eos:test funcall
  (eos:is
   (equal 11
		  (match 10
			(:x :did-not-match)
			((funcall #'(lambda (x)
						  (+ x 1)) r)
			 r)))))

(eos:test or
  (eos:is 
   (equal :matched
		  (match 10
			((or 14 :x)
			 :did-not-match)
			((or 10
				 11)
			 :matched)))))

(eos:test bq
  (eos:is
   (equal 
	'b
	(match '(a b c)
	  ((bq (a (uq x) c))
	   x)))))

(eos:test let
  (eos:is 
   (equal '(a b c)
		  (match (list)
			((let 
				 (x 'a)
  			   (y 'b)
  			   (z 'c))
			 (list x y z))))))

(eos:test must-match-succeed-case
  (eos:is 
   (equal 10
		  (match 10
			((must-match (number x))
			 x)))))

(eos:run!)
