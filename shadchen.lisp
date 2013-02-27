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
	  ((and (listp sub-expressions)
			(= 1 (length sub-expressions))
			(listp (car sub-expressions))
			(= 2 (length (car sub-expressions)))
			(eq 'tail (car (car sub-expressions))))
	   (let ((pattern (cadr (car sub-expressions))))
		 `(match1 ,pattern ,match-value ,@body)))
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
							 ,@body))
			 *match-fail*))))

  (defun match-quote-expander (match-expression match-value body)
	`(if (equalp ,match-expression ,match-value) (progn ,@body) *match-fail*))

  (defun uq? (e)
	(and (listp e)
		 (eq (car e) 'uq)))

  (defun bq->regular-match (bq-expression)
	(let ((sub-expr (cadr bq-expression)))
	  (cond 
		((uq? sub-expr)
		 `(quote ,(cadr sub-expr)))
		((listp sub-expr)
		 `(list ,@(mapcar 
				   (lambda (expr)
					 (cond ((uq? expr)
							(cadr expr))
						   (t `(quote ,expr))))
				   sub-expr)))
		(t
		 sub-expr))))

  (defun match-backquote-expander (match-expression match-value body)
	`(match1 ,(bq->regular-match match-expression) ,match-value ,@body))

  (defun match-and-expander* (sub-expressions match-name body)
	(cond 
	  ((equal sub-expressions '(and*)) `(progn ,@body))
	  (:otherwise 
	   (let ((s1 (cadr sub-expressions)))
		 `(match1 ,s1 ,match-name 
				  (match1 (and* ,@(cddr sub-expressions)) ,match-name ,@body))))))

  (defun match-and-expander (match-expression match-value body)
	(let ((name (gensym "MATCH-AND-EXPANDER-")))
	  `(let ((,name ,match-value))
		 (match1 (and* ,@(cdr match-expression)) ,name ,@body))))

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
	  (declare (ignore val))
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
	`(if (equalp ,match-expression ,match-value) 
		 (progn ,@body)
		 *match-fail*))

  (defun match-literal-number (match-expression match-value body)
	`(if (equalp ,match-expression ,match-value)
		 (progn ,@body)
		 *match-fail*))

  (defun match-literal-character (match-expression match-value body)
	`(if (equalp ,match-expression ,match-value)
		 (progn ,@body)
		 *match-fail*))

  (defun match-literal-keyword (match-expression match-value body)
	`(if (equalp ,match-expression ,match-value)
		 (progn ,@body)
		 *match-fail*))

  (defun match-let-expander (match-expression match-value body)
	(declare (ignore match-value))
	`(let ,(cdr match-expression) ,@body))

  (defun match-or-expander-unsafe (match-expression match-value body)
	(cond 
	  ((length=1 (cdr match-expression))
	   `(match1 ,(cadr match-expression) ,match-value ,@body))
	  (:otherwise
	   (let* ((forms (cdr match-expression))
			  (form (car forms))
			  (rest (cdr forms))
			  (nm (gensym "MATCH-OR-EXPANDER-NM-"))
			  (result-values-list (gensym "result-values-list-"))
			  (result (gensym "result")))
		 `(let* ((,nm ,match-value))
			(let* ((,result-values-list (multiple-value-list (match1 ,form ,nm ,@body)))
				   (,result (car ,result-values-list))) 
			  (if (not (eq *match-fail* ,result))
				  (apply #'values ,result-values-list)
				  (match1 (or ,@rest) ,nm ,@body))))))))

  (defun match-or-expander (match-expression match-value body)
	(assert (apply #'equal-by-binding (cdr match-expression))
			(match-expression)
			"Or sub-expressions ~S contains sub-forms which do not bind identical sets of symbols." match-expression)
	(match-or-expander-unsafe match-expression match-value body))

;;;

  (defun mapcat (f lst)
	(loop for item in lst append (funcall f item)))


  (defun calc-pattern-bindings-extended (expr)
	"Calculate the bound symbols of a user defined pattern."
	(let* ((pattern-args (cdr expr))
		   (pattern-fun (gethash (car expr) *extended-patterns*))
		   (expansion (apply pattern-fun pattern-args)))
	  (calc-pattern-bindings expansion)))

  (defun calc-backquote-bindings (expr)
	"Calculate the bindings for a backquote expression."
	(loop for sub in (cdr expr) 
	   when (and (listp sub)
				 (eq (car sub) 'uq))
	   append 
		 (calc-pattern-bindings (cadr sub))))

  (defun calc-pattern-bindings-list (expr &optional acc)
	(cond ((null expr)
		   acc)
		  ((and (listp expr)
				(listp (car expr))
				(eq 'tail (car (car expr))))
		   (append acc (calc-pattern-bindings (cadr (car expr)))))
		  (t
		   (calc-pattern-bindings-list (cdr expr)
									   (append (calc-pattern-bindings (car expr)) acc)))))
  (defun calc-pattern-bindings (expr)
	"Given a shadchen pattern EXPR return a list of symbols bound
by that expression."
	(cond 
	  ((non-keyword-symbol expr)
	   (list expr))
	  ((vectorp expr)
	   (calc-pattern-bindings `(list ,@(coerce expr 'list))))
	  ((or (not expr)
		   (symbolp expr)
		   (numberp expr)
		   (stringp expr)
		   (characterp expr)) nil)
	  ((extended-patternp (car expr))
	   (calc-pattern-bindings-extended expr))
	  ((listp expr)
	   (case (car expr)
		 (quote nil)
		 ((and values) 
		  (mapcat #'calc-pattern-bindings (cdr expr)))
		 (list (calc-pattern-bindings-list (cdr expr)))
		 (cons (append (calc-pattern-bindings (car expr))
					   (calc-pattern-bindings (cdr expr))))
		 ((? p funcall) (if (= 2 (length expr)) nil
							(calc-pattern-bindings (elt expr 2))))
		 (or (calc-pattern-bindings (cadr expr)))
		 (bq (calc-backquote-bindings expr))
		 ((! must-match string number keyword non-keyword-symbol) (calc-pattern-bindings (cadr expr)))
		 (one-of (calc-pattern-bindings (cadr expr)))
		 (let (mapcar #'car (cdr expr)))
		 (t (error "calc-pattern-bindings: unrecognized pattern ~S." expr))))))

  (defun package-name* (p)
	(if p (package-name p) "no-package-66b73c7f8e8bfa094fa23b4264978ed1"))

	(defun symbol< (s1 s2)
	  (let ((p1 (package-name* (symbol-package s1)))
			(n1 (symbol-name s1))
			(p2 (package-name* (symbol-package s2)))
			(n2 (symbol-name s2)))
		(if (equal p1 p2)
			(string< n1 n2)
			(string< p1 p2))))

	(defun canonical-binding-list (l)
	  (sort l #'symbol<))

	(defun equal-by-binding2 (p1 p2)
	  (equal (canonical-binding-list 
			  (calc-pattern-bindings p1))
			 (canonical-binding-list 
			  (calc-pattern-bindings p2))))

	(defun equal-by-binding (&rest patterns)
	  (cond 
		((= 1 (length patterns)) t)
		((= 2 (length patterns))
		 (equal-by-binding2 (car patterns) (cadr patterns)))
		(t
		 (and (equal-by-binding2 (car patterns) (cadr patterns))
			  (apply #'equal-by-binding (cdr patterns))))))


;;;

	(defun must-match-case (match-expr)
	  (cond 
		((and (listp match-expr)
			  (= 2 (length match-expr)))
		 :pattern-only)
		((and (listp match-expr)
			  (= 4 (length match-expr)))
		 :pattern+)
		(t :unrecognized)))

	(defun match-must-match-expander (match-expr val-expr body)
	  (case (must-match-case match-expr)
		(:pattern-only 
		 (destructuring-bind (_ pattern) match-expr
		   (declare (ignore _))
		   (let ((sym (gensym))) 
			 (match-must-match-expander 
			  `(must-match 
				,pattern 
				,sym 
				(format nil ,(format nil "must-match pattern (~S) failed to match ~~S" pattern) 
						,sym))
			  val-expr body))))
		(:pattern+
		 (destructuring-bind (_ pattern fail-pattern message-expression) match-expr
		   (declare (ignore _))
		   (let ((bound-symbols (calc-pattern-bindings pattern))
			   (value (gensym "must-match-value-"))
			   (result (gensym "must-match-result-")))
		   `(match1 (funcall 
					 (lambda (,value)
					   (let ((,result (match1 ,pattern ,value
											  (list ,@bound-symbols))))
						 (if (eq *match-fail* ,result)
							 (match ,value 
							   (,fail-pattern (let ((,value ,message-expression))
												(if (stringp ,value)
													(error ,value)
													(error "~S" ,value))))
							   (,(gensym)
								 (error 
								  (format nil
										  ,(format nil "must-match pattern (~S) failed and then the failed-value pattern (~S) also failed on value ~~S" 
												   pattern fail-pattern) 
										  ,value))))
							 ,result)))
					 (list ,@bound-symbols))
				,val-expr
			  ,@body))))
		(t (error "Unrecognized must-match pattern form ~S" match-expr))))

;;;


	(defmacro match1 (match-expression match-value &body body)
	  (cond 
		((not match-expression) `(if (not ,match-value) (progn ,@body) *match-fail*))
		((non-keyword-symbol match-expression)
		 (if (or (eq match-expression '_)
				 (eq match-expression '-ignore-))
			 `(progn ,match-value ,@body)
			 `(let ((,match-expression ,match-value))
				,@body)))
		((keywordp match-expression) 
		 (match-literal-keyword match-expression match-value body))
		((stringp match-expression) 
		 (match-literal-string match-expression match-value body))
		((numberp match-expression)
		 (match-literal-number match-expression match-value body))
		((characterp match-expression)
		 (match-literal-character match-expression match-value body))
		((extended-patternp (car match-expression)) 
		 (match-extended-pattern-expander match-expression match-value body))
		((listp match-expression)
		 (case (car match-expression)
		   ((! must-match) (match-must-match-expander match-expression match-value body))
		   (list (match-list-expander match-expression match-value body))
		   (cons (match-cons-expander match-expression match-value body))
		   (quote (match-quote-expander match-expression match-value body))
		   (and (match-and-expander match-expression match-value body))
		   (and* (match-and-expander* match-expression match-value body))
		   ((? p) (match-?-expander match-expression match-value body))
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
			  "MATCH-HELPER: VALUE must be a symbol.  Got ~a." value)
	  (cond 
		((not forms) `(error "No Match for ~s." ,value))
		((listp forms)
		 (let ((first-form (car forms)))
		   (assert (and (listp first-form)
						(> (length first-form) 1))
				   (first-form)
				   "Each MATCH SUB-FORM must be at least two elements long, a matcher
and an expression to evaluate on match. Got ~a instead." first-form)
		   (let ((match-expression (car first-form))
				 (match-body-exprs (cdr first-form))
				 (result-name (gensym "MATCH-HELPER-RESULT-NAME-"))
				 (result-values-name (gensym "MATCH-HELPER-RESULT-VALUES-NAME-")))
			 `(let* ((,result-values-name (multiple-value-list (match1 ,match-expression ,value ,@match-body-exprs)))
					 (,result-name (car ,result-values-name)))
				(if (not (eq *match-fail* ,result-name)) (apply #'values ,result-values-name)
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

  (defpattern number (&optional (pattern '_))
	`(p #'numberp ,pattern))

  (defpattern symbol (&optional (pattern '_))
	`(p #'symbolp ,pattern))

  (defpattern string (&optional (pattern '_))
	`(p #'stringp ,pattern))

  (defpattern non-kw-symbol (&optional (pattern '_))
	(let ((val (gensym)))
	  `(p #'(lambda (,val)
			  (and (symbolp ,val)
				   (not (keywordp ,val))))
		  ,pattern)))

  (defpattern keyword (&optional (pattern '_))
	`(p #'keywordp ,pattern))


  (defun htbl-fetcher (key)
	#'(lambda (htbl) (gethash key htbl)))

  (defmacro named-let (name binders &body body)
	`(labels ((,name ,(mapcar #'car binders) ,@body))
	   (,name ,@(mapcar #'cadr binders))))

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
  `(and (? #'hash-table-p)
		,@(named-let recur 
					 ((pairs key/pat-pairs)
					  (acc nil))
					 (match pairs
					   (nil (reverse acc))
					   ((cons key (cons pat rest))
						(recur rest
							   (cons `(funcall (htbl-fetcher ,key) ,pat) acc)))))))

(defpattern let1 (name value)
  `(let (,name ,value)))

(defmacro match-let* (bindings &body body)
  "Just like let* but each symbol part of each binding can be a match
expression of arbitrary complexity."
  (match bindings 
	((list) `(progn ,@body))
	((cons (list pattern value) rest-bindings)
	 `(match ,value 
		(,pattern 
		 (match-let* ,rest-bindings ,@body))))))

(defmacro match-let (bindings &body body)
  "Just like let* but each symbol part of each binding can be a match
expression of arbitrary complexity."
  (let ((patterns (mapcar #'car bindings))
		(values (mapcar #'cadr bindings)))
	`(match (list ,@values)
	   ((list ,@patterns) ,@body))))

(defmacro match-loop (recur-point bindings &body body)
  "Like match-let but the binding form can be re-entered by calling
a local function indicated by `recur-point` with the same number of arguments
as bindings expressions in BINDINGS."
  (let ((args 
		 (loop for i from 1 to (length bindings)
			collect (gensym "match-loop-arg-")))
		(patterns (mapcar #'car bindings))
		(initial-values 
		 (mapcar #'cadr bindings)))
	`(labels ((,recur-point ,args
				(match (list ,@args)
				  ((list ,@patterns) ,@body))))
	   (,recur-point ,@initial-values))))

(defvar *match-function-table* (make-hash-table))
(defun extend-defun-match-table (name lexpr)
  (let ((c (reverse (gethash name *match-function-table*))))
	(setf (gethash name *match-function-table*)
		  (reverse (cons lexpr c)))))

(defmacro defun-match- (name patterns &body body)
  (let ((args (gensym))
		(funs (gensym))
		(fun (gensym))
		(rval (gensym))
		(loopf (gensym))
		(compound-name (intern (format nil "~S (~S)" name patterns)))
		(doc-string (if (stringp (car body)) (car body) "")))
	`(progn 
	   (defun ,name (&rest ,args)
		 (named-let ,loopf ((,funs (gethash ',name *match-function-table*)))
		   (if (null ,funs)
			   (error "~S: match fail for ~S." ',name ,args)
			   (let* ((,fun (car ,funs))
					  (,funs (cdr ,funs))
					  (,rval (apply ,fun ,args)))
				 (if (eq *match-fail* ,rval)
					 (,loopf ,funs)
					 ,rval)))))
	   (defun ,compound-name (&rest ,args)
		 ,doc-string
		 (match1 (list ,@patterns) ,args ,@body))
	   (setf (gethash ',name *match-function-table*)
			 (list #',compound-name)))))

(defmacro defun-match (name patterns &body body)
  (let ((compound-name (intern (format nil "~S (~S)" name patterns)))
		(args (gensym))
		(doc-string (if (stringp (car body)) (car body) "")))
	`(progn 
	   (defun ,compound-name (&rest ,args)
		 ,doc-string
		 (match1 (list ,@patterns) ,args
		   ,@body))
	   (extend-defun-match-table ',name #',compound-name))))

#|

(match 'y 
  ((and (symbol x) 
		(must-match 'z a (format nil "Failed, but got ~S" a)))
   :hey))

(defun-match- my-prod (anything)
  (my-prod anything 1))
(defun-match my-prod (nil acc)
  acc)
(defun-match my-prod ((list (must-match x f (format nil "failed on ~s" f)) (tail rest)) acc)
  (my-prod rest (* acc x)))
(my-prod '(1 2 3 4))



(match1 (list (must-match (number x)) (tail tl)) '(1 2 3) tl)

(match-loop rec 
	(((list x y) (list 0 0)))
  (if (< (+ x y) 100) 
	  (rec (list (+ x 1) (+ y x)))
	  (list x y)))

(match 10
	((or (number val)
		 (string s))
	 val))

(match (list 1 2) (x x))

(match (list 1 2 3 4)
  ((list a b (tail c)) c))

(calc-pattern-bindings 
					   '(and a b c))

|#

;; (let ((ht (make-hash-table :test #'equal)))
;;   (labels ((add (key val) (setf (gethash key ht) val)))
;; 	(add :x 10)
;; 	(add :y (list 1 2))
;; 	(add :z 'a-value)
;; 	(match ht
;; 	  ((hash-table 
;; 		:x x
;; 		:y (list y z))
;; 	   (list x y z)))))


;;; "shadchen" goes here. Hacks and glory await!

