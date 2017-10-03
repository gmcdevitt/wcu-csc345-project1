;; Gerard McDevitt
;; CSC 345
;; Project 1 - Lisp Symbolic Differentiation
;; Will differentiate any expression (supplied as a list) with a variable (differentiate F V) 
;; Supplied function must only contain any combination of:
;;	- Addition
;;	- Subtraction
;; 	- Multiplication
;;	- Division
;; 	- Exponentials

;; (load "C://Users/Gerard/OneDrive/Personal/Code/West Chester/ProgramParadigms/Lisp/Lisp Project 1/piotrproject.lisp")
;; (load "C://Users/Gerard/Downloads/test-functions.lisp")

(defun differentiate (F V)
"Main, fully abstracted differentiate function, calls accessors based on predicates defined below"
  (cond ((constant-p F) (make-constant 0))
	((variable-p F) (if (equal (make-variable F) (make-variable V))
			    (make-constant 1)
			    (make-constant 0)))
	((negation-p F) (if (evenp (negation-p-count F)) (differentiate (last F) V) (make-negative (differentiate (negation-operand F) V))))
	((sum-p F) (make-sum (differentiate (first-sum-operand F) V)
			     (differentiate (second-sum-operand F) V)))
	((difference-p F) (make-difference (differentiate (first-difference-operand F) V)
					   (differentiate (second-difference-operand F) V)))
	((product-p F)(make-sum(make-product(multiplicand F)(differentiate(multiplier F)V))
			       (make-product(multiplier F)(differentiate(multiplicand F) V))))
	((quotient-p F) (make-quotient (make-difference 
					(make-product (divisor F)
						      (differentiate (dividend F) V)) 
					(make-product (dividend F)
						      (differentiate (divisor F) V)))
				       (make-product (divisor F)(divisor F))))
	((power-p F)(make-product (make-power 
				   (power-operand-1 F)(- (power-operand-2 F) 1)) (power-operand-2 F)))))



; Symbols 												--
(defconstant constant-symbols '(A B C D E F G H M N))
(defconstant variable-symbols '(U V W X Y Z))
(defconstant addition-symbol '+)
(defconstant subtraction-symbol '-)
(defconstant multiplication-symbol '*)
(defconstant division-symbol '/)
(defconstant power-symbol '**)
; End Symbols											-- 

; Operators 								--
(defun negation-operator (F)(first F))
(defun sum-operator (F)(second F))
(defun difference-operator (F) (second F))
(defun product-operator (F) (second F))
(defun quotient-operator (F) (second F))
(defun power-operator (F) (second F))
; End Operators								--


(defun first-sum-operand (F)
  "Will get the first operand of an addition function (the first item in the list)"
  (first F))

(defun second-sum-operand (F) 
  "Will get the second operand of an addition function (the last item in the list)"
  (third F)) 

(defun first-difference-operand (F) 
    "Will get the first operand of an subtraction function (the first item in the list)"
  (first F))

(defun second-difference-operand (F) 
  "Will get the second operand of an subtraction function (the last item in the list)"
  (third F))

(defun multiplicand (F) 
  "Returns the multiplicand of a multiplication function. (the first item in the list)"
  (first F))

(defun multiplier (F) 
  "Returns the multiplier of a multiplication function. (the last item in the list)"
  (third F))

(defun dividend (F) 
    "Returns the dividend of a division function. (the first item in the list)"
  (first F))

(defun divisor (F) 
  "Returns the divisor of a multiplication function. (the last item in the list)"
  (third F))
			
(defun negation-operand (F)
	"Gets the last item in the list, this should only be called if the function is deemed to be negation by negation-p"
	(last F))
	 
;;;Power-operand-1
(defun power-operand-1 (F) 
  "Returns the first operand of an exponential function."
  (first F))
;;;Power-operand-2
(defun power-operand-2 (F) 
  "Returns the third operand of an exponential function."
  (third F))

;;;========================================================================
;;; PREDICATES ***********************************************************

(defun negation-p (F) 
	(cond 	((endp F) nil)
			((equal subtraction-symbol (first F)) (negation-p (rest F)))
			((equal (rest F) nil) (if (or (constant-p F) (variable-p F)) 1 nil))))
			
(defun negation-p-count (L)
    (labels ((negation-p-aux (L Acc)
    	        (cond	((equal (first L) subtraction-symbol) (negation-p-aux (rest L) (+ 1 Acc)))
     		            ((and (variable-p (list (first L))) (equal 0 (length (rest L)))) Acc)
     		            ((and (constant-p (list (first L))) (equal 0 (length (rest L)))) Acc))))
    (negation-p-aux L 0)))

(defun constant-p (F)
  "Returns true if F is one of the constant-symbols"
   (or  (numberp F)
	(member F *constant-symbols*)))

(defun variable-p (F)
  "Returns true if F is one of the variable-symbols."
  (member F *variable-symbols*))

(defun sum-p (F) 
  "Returns true if F is the sum-symbol."
  (and (listp F)
	(>= (length F) 3)
	(equal (sum-operator F)*sum-symbol*)))

(defun difference-p (F)
  "Returns true if the first member of F is a negation-symbol but second is not."
  (and (listp F)
	(>= (length F) 3)
	(equal (difference-operator F)*difference-symbol*)))

(defun product-p (F)
  "Returns true if the first member of F is the product-symbol."
  (and (listp F)
	(>= (length F) 3)
	(equal (product-operator F)*product-symbol*)))

(defun quotient-p (F)
  "Returns true if the first member of F is the quotient-symbol."
  (and (listp F)
	(>= (length F) 3)
	(equal (quotient-operator F)*quotient-symbol*)))

(defun power-p (F)
  "Returns true if the first member of F is the power-symbol."
   (and (listp F)
	(>= (length F) 3)
	(equal (power-operator F)*power-symbol*)))

;;;==============================================================

;;; CONSTRUCTORS************************************************
;;;trivial variable constructor
(defun make-variable (V) 
  "Returns V."
  V)

(defun make-constant (C)
  "Returns the constant."
  C)
  
(defun make-sum (F G)
  "This returns a list with the in the format (F + G)
   If F and G are numbers it just adds them.
   Also if they are opposite just return zero"
  (cond ((eq 0 F) G)
	((eq 0 G) F)
	((and (numberp F) (numberp G)) (+ F G)) 
	(t (list F *sum-symbol* G)))) 

(defun make-difference (F G)
  "This returns a list with the format (F - G)
   If F and G are numbers then subtract them.
   Also if they are equal just return zero"
  (cond ((eq 0 F) (make-negation G))
	((eq 0 G) F)
	((equal F G) 0)
	((and (numberp F)(numberp G)) (- F G))
	(t (list F  *negation-symbol* G))))
  
(defun make-product (F G)
  "This returns a list with the format (F * G)
   If F and G are numbers then multiplication is performed.
   If either F or G is 1 return the other.
   If either is zero return zero"
  (cond ((eq 1 F) G)
	((eq 1 G) F)
	((eq 0 F) 0)
	((eq 0 G) 0)
	((and (numberp F)(numberp G)) (* F G))
	(t (list F  *product-symbol* G))))


(defun make-quotient (F G)
  "This returns a list with the format of (F / G)
   If F and G are numbers then it divides them."
  (cond ((eq 0 F) 0)
        ((eq 0 G) nil) 
	((and (numberp F) (numberp G)) (/ F G))
	(t (list F  *quotient-symbol* G))))

(defun make-power (F G)
  "This returns a list with the format of (F ** G)
   If F and G are numbers it just makes it the exponential."
  (cond ((and (numberp F) (numberp G)) (expt F G)) 
        (t (list F  *power-symbol* G))))

(defun make-negative (F)
	(list subtraction-symbol F))
	 
	
;;;================= Test Functions =================		
(defun run-all-diffs (n)
  "Run differentiate on all functions from F1 to F<n>"
  (terpri)
  (run-all-diffs2 1 n))


(defun run-all-diffs2 (m n)
  "Helper function for RUN-DIFFS -- evaluate NICE-DIIF on F<m>--F<n>"
  (cond ((> m n) nil)
	(t (make-and-eval-differentiate-expression m)
	   (run-all-diffs2 (1+ m) n))))

(defun build-symbol (n)
  "Makes (and interns) the lisp symbol F<n>, for example: F1 or F2 or F3, etc. "
  (intern (concatenate 'string "F" (prin1-to-string n))))

(defun make-and-eval-differentiate-expression (n)
  "Build and evaluate the expression for differentiating F<n> w.r.t. variable x (or y if n=6)"
  (format t "DIFFERENTIATING FUNCTION F~a" n)
  (if (= n 6)(format t "               NB: differentiate here w.r.t. y, not x"))
  (terpri)
  (eval `(nice-diff ,(build-symbol n) (if (= ,n 6) 'y 'x))))

(defun nice-diff (F V)
  (format t "FUNCTION: ~a~%VARIABLE: ~a~%  RESULT: ~A~%~%" F V (differentiate F V)))

;;;===================================================================
;;;  THE TEST EXPRESSIONS

(defconstant f1 'a)
(defconstant f2 '(x / 3))
(defconstant f3 '(- x))
(defconstant f4 '(- - x))
(defconstant f5 '(- - - x))        
(defconstant f6 '(x / 5))                
(defconstant f7 '(x + x))
(defconstant f8 '(- - (x + x)))
(defconstant f9 '((x + x) + x))
(defconstant f10 '(x - (- x)))
(defconstant f11 '(4 * x))
(defconstant f12 '(x * x))
(defconstant f13 '((- x) + (- x)))
(defconstant f14 '(x / x))
(defconstant f15 '(x / (2 * x)))
(defconstant f16 '((x * x) * (x * x)))
(defconstant f17 '((4 * x) - (2 * x)))
(defconstant f18 '(3 * (x ** 4)))
(defconstant f19 '(a * (x ** 3)))
(defconstant f20 '(-2 * (x ** -3)))
