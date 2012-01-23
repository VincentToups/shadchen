;;;; shadchen.lisp

;;; Copyright 2012, Vincent Toups
;;; This program is distributed under the terms of the GNU Lesser 
;;; General Public License (see license.txt).

(in-package #:shadchen)



(defstruct match-fail-struct)

(defvar *match-fail* (make-match-fail-struct))

(eval-when (:compile-toplevel :load-toplevel :execute)

  (defun non-keyword-symbol (o)
	(and o
		 (symbolp o)
		 (not (keywordp o))))

  (defun match-list-expander* (sub-expressions match-value body)
	(cond 
	  ((not sub-expressions) `(if (not ,match-value) (progn ,@body) *match-fail*))
	  (:otherwise
	   (let ((first-expression (car sub-expressions))
			 (list-name (gensym "MATCH-LIST-EXPANDER*-")))
		 `(let ((,list-name ,match-value))
			(if (and (listp ,list-name)
					 ,list-name)
				(match1 ,first-expression (car ,list-name)
				  (match1 (list ,@(cdr sub-expressions)) (cdr ,list-name) 
					,@body))
				*match-fail*))))))

  (defun match-list-expander (match-expression match-value body)
	(match-list-expander* (cdr match-expression) match-value body))


  (defun match-cons-expander (match-expression match-value body)
	(let ((car-match (elt match-expression 1))
		  (cdr-match (elt match-expression 2))
		  (name (gensym "MATCH-CONS-EXPANDER-")))
	  `(let ((,name ,match-value))
		 (if (listp ,name)
			 (match1 ,car-match (car ,name)
			   (match1 ,cdr-match (cdr ,name)
				 ,@body))))))

  (defun match-quote-expander (match-expression match-value body)
	`(if (equalp ,match-expression ,match-value) (progn ,@body) *match-fail*))

  (defun match-backquote-expander (match-expression match-value body)
	(let ((datum (cadr match-expression)))
	  (cond 
		((not datum) `(progn ,@body))
		((and (listp datum)
			  (eq (car datum) 'uq))
		 (let ((sub-match (cadr datum)))
		   `(match1 ,sub-match ,match-value ,@body)))
		((listp datum)
		 (let ((first-qt (car datum))
			   (rest-bq (cdr datum))
			   (name (gensym "MATCH-BACKQUOTE-EXPANDER-")))
		   `(let ((,name ,match-value))
			  (if (and ,name
					   (listp ,name))
				  (match1 (bq ,first-qt) (car ,name)
					(match1 (bq ,rest-bq) (cdr ,name) ,@body))
				  *match-fail*))))
		(:otherwise 
		 `(match1 ',datum ,match-value ,@body)))))

  (defun match-and-expander* (sub-expressions match-name body)
	(cond 
	  ((not sub-expressions) `(progn ,@body))
	  (:otherwise 
	   (let ((s1 (car sub-expressions))
			 (name (gensym "MATCH-AND-EXPANDER*-")))
		 `(match1 ,s1 ,match-name 
			(match1 (and ,@(cdr sub-expressions)) ,match-name ,@body))))))

  (defun match-and-expander (match-expression match-value body)
	(let ((name (gensym "MATCH-AND-EXPANDER-")))
	  `(let ((,name ,match-value))
		 ,(match-and-expander* (cdr match-expression) name body))))

  (defun match-?-expander (match-expression match-value body)
	(let ((name (gensym "MATCH-?-EXPANDER-NAME-"))
		  (f-name (gensym "MATCH-?-EXPANDER-FUNCTION-")))
	  (case (length (cdr match-expression))
		(0 (error "MATCH1: MATCH-?-EXPANDER: zero arguments to MATCH-?-EXPANDER.  Needs 1 or 2."))
		(1 `(let ((,name ,match-value)
				  (,f-name ,(cadr match-expression)))
			  (if (funcall ,f-name ,name) (progn ,@body) *match-fail*)))
		(2 `(let ((,name ,match-value)
				  (,f-name ,(cadr match-expression)))
			  (if (funcall ,f-name ,name) (match1 ,(elt match-expression 2) ,name ,@body)
				  *match-fail*)))
		(otherwise
		 (error "MATCH-?-EXPANDER: MATCH-?-EXPANDER takes only 1 or 2 arguments.")))))

  (defun match-values-expander (match-expression match-value body)
	(let ((name (gensym "MATCH-VALUES-EXPANDER-")))
	  `(let ((,name (multiple-value-list ,match-value)))
		 (match1 (list ,@(cdr match-expression)) ,name ,@body))))

  (defun match-funcall-expander (match-expression match-value body)
	(assert (and (listp match-expression) (= 3 (length match-expression)))
			(match-expression)
			"MATCH-FUNCALL-EXPANDER: FUNCALL match expression must have
two terms, a function and a match against the result.  Got
~a." match-expression)
	(let ((name (gensym "MATCH-FUNCALL-EXPANDER-NAME-"))
		  (fun-name (gensym "MATCH-FUNCALL-EXPANDER-FUN-NAME-"))
		  (result-name (gensym "MATCH-FUNCALL-EXPANDER-RESULT-NAME-")))
	  `(let* ((,name ,match-value)
			  (,fun-name ,(cadr match-expression))
			  (,result-name (funcall ,fun-name ,name)))
		 (match1 ,(caddr match-expression) ,result-name ,@body))))

  (defvar *extended-patterns* (make-hash-table) "Holds user declared patterns.")
  (defun extended-patternp (pattern-head) 
	"Return T if PATTERN-HEAD indicates a user provided pattern."
	(multiple-value-bind (val in) (gethash pattern-head *extended-patterns*)
	  in))

  (defun match-extended-pattern-expander (match-expression match-value body)
	(let* ((pattern-args (cdr match-expression))
		   (pattern-fun (gethash (car match-expression) *extended-patterns*))
		   (expansion (apply pattern-fun pattern-args)))
	  `(match1 ,expansion ,match-value ,@body)))

  (defmacro defpattern (name args &body body)
	`(setf (gethash ',name *extended-patterns*)
		   #'(lambda ,args ,@body)))

  (defun match-literal-string (match-expression match-value body)
	`(if (string= ,match-expression ,match-value) 
		 (progn ,@body)
		 *match-fail*))

  (defun match-literal-number (match-expression match-value body)
	`(if (= ,match-expression ,match-value)
		 (progn ,@body)
		 *match-fail*))

  (defun match-literal-keyword (match-expression match-value body)
	`(if (eq ,match-expression ,match-value)
		 (progn ,@body)
		 *match-fail*))

  (defun match-let-expander (match-expression match-value body)
	`(let ,(cdr match-expression) ,@body))

  (defun match-or-expander (match-expression match-value body)
	(cond 
	  ((length=1 (cdr match-expression))
	   `(match1 ,(cadr match-expression) ,match-value ,@body))
	  (:otherwise
	   (let* ((forms (cdr match-expression))
			  (form (car forms))
			  (rest (cdr forms))
			  (nm (gensym "MATCH-OR-EXPANDER-NM-")))
		 `(let* ((,nm ,match-value)
				 (result (match1 ,form ,nm ,@body)))
			(if (not (eq *match-fail* result))
				result
				(match1 (or ,@rest) ,nm ,@body)))))))


  (defmacro match1 (match-expression match-value &body body)
	(cond 
	  ((not match-expression) `(if (not ,match-value) (progn ,@body) *match-fail*))
	  ((non-keyword-symbol match-expression)
	   `(let ((,match-expression ,match-value))
		  ,@body))
	  ((keywordp match-expression) 
	   (match-literal-keyword match-expression match-value body))
	  ((stringp match-expression) 
	   (match-literal-string match-expression match-value body))
	  ((numberp match-expression)
	   (match-literal-number match-expression match-value body))
	  ((extended-patternp (car match-expression)) 
	   (match-extended-pattern-expander match-expression match-value body))
	  ((listp match-expression)
	   (case (car match-expression)
		 (list (match-list-expander match-expression match-value body))
		 (cons (match-cons-expander match-expression match-value body))
		 (quote (match-quote-expander match-expression match-value body))
		 (and (match-and-expander match-expression match-value body))
		 (? (match-?-expander match-expression match-value body))
		 (funcall (match-funcall-expander match-expression match-value body))
		 (or (match-or-expander match-expression match-value body))
		 (bq (match-backquote-expander match-expression match-value body))
		 (values (match-values-expander match-expression match-value body))
		 (let (match-let-expander match-expression match-value body))
		 (otherwise (error "MATCH1: Unrecognized match-expression ~a" match-expression))))
	  (:otherwise
	   (error "MATCH1: Unrecognized match-expression ~a" match-expression))))

  (defmacro match-helper (value &body forms)
	(assert (symbolp value)
			(value)
			"MATCH-HELPER: VALUE must be a symbol!  Got ~a." value)
	(cond 
	  ((not forms) `(error "No Match for ~a!" ,value))
	  ((listp forms)
	   (let ((first-form (car forms)))
		 (assert (and (listp first-form)
					  (> (length first-form) 1))
				 (first-form)
				 "Each MATCH SUB-FORM must be at least two elements long, a matcher
and an expression to evaluate on match. Got ~a instead." first-form)
		 (let ((match-expression (car first-form))
			   (match-body-exprs (cdr first-form))
			   (result-name (gensym "MATCH-HELPER-RESULT-NAME-")))
		   `(let ((,result-name 
				   (match1 ,match-expression ,value ,@match-body-exprs)))
			  (if (not (eq *match-fail* ,result-name)) ,result-name
				  (match-helper ,value ,@(cdr forms)))))))))


  (defmacro match (value &body forms)
	"Attempt to match VALUE against each of the patterns in the CAR of
FORMS.  When a match is detected, its subsequent forms are executed as
in a PROGN where the bindings implied by the match are in effect.  

An error is thrown when no matches are found."
	(let ((name (gensym "MATCH-VALUE-NAME-")))
	  `(let ((,name ,value)) 
		 (match-helper ,name ,@forms))))


  (defmacro match-lambda (&body forms) 
	"Like MATCH except the VALUE is curried."
	(let ((name (gensym "MATCH-LAMBDA-NAME-")))
	  `(function (lambda (,name) (match ,name ,@forms)))))

  (defun length=1 (lst)
	"Returns T when LST has one element."
	(and (consp lst)
		 (not (cdr lst)))))

(defpattern list-rest (&rest patterns)
  (if (length=1 patterns)
	  `(? #'listp ,(car patterns))
	  (let ((pat (car patterns))
			(pats (cdr patterns)))
		`(and (funcall #'car ,pat)
			  (funcall #'cdr 
					   (list-rest ,@pats))))))

(defun htbl-fetcher (key)
  #'(lambda (htbl) (gethash key htbl)))

(defmacro named-let (name binders &body body)
  `(labels ((,name ,(mapcar #'car binders) ,@body))
	 (name ,@(mapcar #'cadr binders))))

(defpattern struct (struct-name &rest fields)
  `(and
	(? #'(lambda (o)
		   (typep o ',struct-name)))
	,@(loop for f in fields collect
		   `(funcall 
			 #'(lambda (o)
				 (slot-value o ',(car f)))
			 ,(cadr f)))))

(defpattern hash-table (&rest key/pat-pairs)
  `(and (? #'hash-tablep)
		,@(named-let recur 
					 ((pairs key/pat-pairs)
					  (acc nil))
					 (match pairs
					   (nil (reverse acc))
					   ((cons key (cons pat rest))
							 (recur rest
									(cons `(funcall (htbl-fetch ,key) ,pat) acc)))))))

(defpattern let1 (name value)
  `(let (,name ,value)))

(define-test match1
	(assert-equal *match-fail* (match1 (list x y) (list 10 11 13) (+ x y)))
  (assert-equal '(10 (11 12 13)) (match1 (cons x y) (list 10 11 12 13) (list x y)))
  (assert-equal '(10 11 12) (match1 (list x (list y z)) (list 10 (list 11 12)) (list x y z)))
  (assert-equal '(10 12) (match1 (list x (list 'y z)) (list 10 (list 'y 12)) (list x z)))
  (assert-equal 'success (match1 (bq (x)) (list 'x) 'success))
  (assert-equal 'success (match1 (bq (x y z)) (list 'x 'y 'z) 'success))
  (assert-equal '(10 11) 
				(match1 (bq x (uq (list a b)) z)
						(list 'x (list 10 11) 'z) 
						(list a b)))
  (assert-equal '((10 11) 10 11) (match1 (and x (list a b)) (list 10 11) (list x a b)))
  (assert-equal 10 (match1 (? #'numberp x) 10 x))
  (assert-equal *match-fail* (match1 (? #'numberp x) "cat" x))
  (assert-equal 21 (match1 (values x y) (values 10 11) (+ x y )))
  (assert-equal 10 (match1 (funcall #'car x) (cons 10 11) x))
  (assert-equal '(10 (11 12))
				(match1 (list-rest x y) (list 10 11 12) (list x y)))
  (assert-error 'simple-error
				(match1 (?) "" ""))
  (assert-equal 10 (match1 (let (x 10)) 'any-val x))
  (assert-equal "x" (match1 (or (? #'stringp x) 
								(? #'numberp x)) "x" x))
  (assert-equal 10 (match1 (or (? #'stringp x) 
							   (? #'numberp x)) 10 x))

  (assert-error 'simple-error
				(match1 (? a b c) "" "")))

(define-test match
  (assert-equal
   5 (match (list 1 2 3)
	   ((list x y) (+ x y))
	   ((list x y z) (+ y z))))
  (assert-error 'simple-error
				(match (list 1 2 3)
				  ((list x) x)
				  ((list a b) (+ a b))))
  (assert-equal 'string
				(match "test"
				  ((? #'numberp) 'number)
				  ((? #'stringp) 'string))))

(run-tests)


;;; "shadchen" goes here. Hacks and glory await!

