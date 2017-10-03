;; Gerard McDevitt
;; CSC 345
;; Project 1 - Lisp Symbolic Differentiation
;; Will differentiate any expression F (supplied as a list) with a variable V (differentiate F V) 
;; Supplied function must only contain any combination of:
;;	- Addition
;;	- Subtraction
;; 	- Multiplication
;;	- Division
;; 	- Exponentials

(defun differentiate (F V)
	"Main, fully abstracted differentiate function, calls accessors based on predicates defined below"
	(cond	((constant-p F) (make-item 0))
			((variable-p F) (if (equal (make-item F) (make-item V))(make-item 1)(make-item 0)))
			((negation-p F) (make-negation (differentiate (negation-operand F) V)))
			((addition-p F) (make-addition (differentiate (first-addition-operand F) V)(differentiate (second-addition-operand F) V)))
			((subtraction-p F) (make-subtraction (differentiate (first-subtraction-operand F) V)(differentiate (second-subtraction-operand F) V)))
			((multiplication-p F)(make-addition(make-multiplication(multiplicand-operator F)(differentiate(multiplier-operand F)V))(make-multiplication(multiplier-operand F)(differentiate(multiplicand-operator F) V))))
			((division-p F) (make-division (make-subtraction (make-multiplication (divisor-operand F) (differentiate (dividend-operand F) V)) (make-multiplication (dividend-operand F)(differentiate (divisor-operand F) V)))(make-multiplication (divisor-operand F)(divisor-operand F))))
			((exponent-p F)(make-multiplication (make-exponent (first-exponent-operand F)(- (second-exponent-operand F) 1)) (second-exponent-operand F)))))


; Symbols
(defconstant *variable-symbols* '(U V W X Y Z))
(defconstant *constant-symbols* '(A B C D E F G H N M))
(defconstant *addition-symbol*       '+)
(defconstant *negation-symbol*  '-)
(defconstant *multiplication-symbol*   '*)
(defconstant *division-symbol*  '/)
(defconstant *exponent-symbol*     '**)
(defconstant *subtraction-symbol* '-)
; Symbols end

; Operators 
(defun negation-operator (F)(first F))
(defun addition-operator (F)(second F))
(defun subtraction-operator (F) (second F))
(defun multiplication-operator (F) (second F))
(defun division-operator (F) (second F))
(defun exponent-operator (F) (second F))
; Operators End
  
; Operands
(defun first-addition-operand (F)
  "Will get the first operand of an addition function (the first item in the list)"
  (first F))

(defun second-addition-operand (F) 
  "Will get the second operand of an addition function (the last item in the list)"
  (third F)) 

(defun first-subtraction-operand (F) 
    "Will get the first operand of an subtraction function (the first item in the list)"
  (first F))

(defun second-subtraction-operand (F) 
  "Will get the second operand of an subtraction function (the last item in the list)"
  (third F))

(defun multiplicand-operator (F) 
  "Returns the multiplicand of a multiplication function. (the first item in the list)"
  (first F))

(defun multiplier-operand (F) 
  "Returns the multiplier of a multiplication function. (the last item in the list)"
  (third F))

(defun dividend-operand (F) 
    "Returns the dividend of a division function. (the first item in the list)"
  (first F))

(defun divisor-operand (F) 
  "Returns the divisor of a multiplication function. (the last item in the list)"
  (third F))
			
(defun negation-operand (F)
 "Returns the operand of a negative: second if the length is two, else everything after the negation-symbol."
  (if (= (length F) 2)(second F)(rest F)))
	 
(defun first-exponent-operand (F) 
  "Returns the first operand of an exponential function, or the base."
  (first F))

(defun second-exponent-operand (F) 
  "Returns the third operand of an exponential function, or the factor."
  (third F))
; Operands end

; Predicates
(defun constant-p (F)
  "Returns true if F is a constant-symbol"
   (or  (numberp F)
	(member F *constant-symbols*)))

(defun variable-p (F)
  "Returns true if F is a variable-symbol."
  (member F *variable-symbols*))

(defun addition-p (F) 
  "Returns true if only the second item of the list is the addition symbol"
  (and (listp F) (>= (length F) 3) (equal (addition-operator F)*addition-symbol*)))

(defun subtraction-p (F)
  "Returns true if only the second item of the list is the subtraction symbol"
  (and (listp F) (>= (length F) 3) (equal (subtraction-operator F)*subtraction-symbol*)))

(defun multiplication-p (F)
  "Returns true if only the second item of the list is the multiplication symbol"
  (and (listp F) (>= (length F) 3)(equal (multiplication-operator F)*multiplication-symbol*)))

(defun division-p (F)
  "Returns true if only the second item of the list is the multiplication symbol"
  (and (listp F) (>= (length F) 3) (equal (division-operator F)*division-symbol*)))
  
(defun negation-p (F)
  "Checks if supplied function has two items and the first is the negation symbol or if all items in the function besides the last are the negation symbol"
  (or (and(= (length F) 2)(equal (negation-operator F) *negation-symbol*)) (and (equal (negation-operator F) *negation-symbol*)(negation-p (rest f)))))

(defun exponent-p (F)
  "Returns true if only the second item of the list is the exponent symbol"
  (and (listp F) (>= (length F) 3) (equal (exponent-operator F)*exponent-symbol*)))
; Predicates end

; Constructors
(defun make-item (I) 
  "Returns I. Only use for Constants and Variables"
  I)
  
(defun make-addition (F G)
  "Create and return a list with format (F + G)
   Will return the answer instead of a list if F and G are both numbers"
  (cond ((eq 0 F) G)
		((eq 0 G) F)
		((and (numberp F) (numberp G)) (+ F G)) 
		(t (list F *addition-symbol* G)))) 

(defun make-subtraction (F G)
  "Create and return a list with format (F - G)
   Will return the answer instead of a list if F and G are both numbers"
  (cond ((eq 0 F) (make-negation G))
		((eq 0 G) F)
		((equal F G) 0)
		((and (numberp F)(numberp G)) (- F G))
		(t (list F  *negation-symbol* G))))
  
(defun make-multiplication (F G)
  "Create and return a list with format (F * G)
   Will return the answer instead of a list if F and G are both numbers"
  (cond ((eq 1 F) G)
		((eq 1 G) F)
		((eq 0 F) 0)
		((eq 0 G) 0)
		((and (numberp F)(numberp G)) (* F G))
		(t (list F  *multiplication-symbol* G))))


(defun make-division (F G)
  "Create and return a list with format (F / G)
   Will return the answer instead of a list if F and G are both numbers"
  (cond ((eq 0 F) 0)
        ((eq 0 G) nil) 
		((and (numberp F) (numberp G)) (/ F G))
		(t (list F  *division-symbol* G))))

(defun make-negation (F)
  "Create and return a list with format (- F)
   Will also attempt to simplify the list if there are duplicate negation symbols eg (- - 2) --> 2 and (- - - 2) --> (- 2)
   If the simplification fails, it simply adds a negation symbol to the front of the list."
   (cond ((numberp F) (* -1 F))
		 ((and (negation-p F)(evenp (length F))) (negation-operand F))
		 ((and (negation-p F)(oddp (length F))) (list *negation-symbol* (negation-operand F)))
		 ( t  (list *negation-symbol* F)))) ;; This catch all should never be hit

 (defun make-exponent (F G)
  "This returns a list with format (F ** G)
  Will return the answer instead of a list if F and G are both numbers"
  (cond ((and (numberp F) (numberp G)) (expt F G)) 
        (t (list F  *exponent-symbol* G))))	
; End Constructors
	