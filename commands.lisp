(differentiate 'A 'X)
(differentiate '(X / 3) 'X)
(differentiate '(- X) 'X)
(differentiate '(- - X) 'X)
(differentiate '(- - - X) 'X)
(differentiate '(X / 5) 'X)
(differentiate '(x + x) 'X)
(differentiate '(- - (x + x)) 'X)
(differentiate '((x + x) + x) 'X)
(differentiate '(x - (- x)) 'X)   			WRONG
(differentiate '(4 * x) 'X)
(differentiate '(x * x) 'X) 				WRONG
(differentiate '((- x) + (- x)) 'X) 		WRONG
(differentiate '(x / x) 'X)
(differentiate '(x / (2 * x)) 'X)
(differentiate '((x * x) * (x * x)) 'X)     WRONG
(differentiate '((4 * x) - (2 * x)) 'X)		WRONG
(differentiate '(3 * (x ** 4)) 'X)			WRONG
(differentiate '(a * (X ** 3)) 'X)			WRONG
(differentiate '(- 2 * (x ** - 3)) 'X)		WRONG
