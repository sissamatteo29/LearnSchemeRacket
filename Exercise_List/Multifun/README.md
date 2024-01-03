Design a construct to define multiple functions with the same number of arguments at the same time. 
The proposed syntax is the following: 

(multifun <list of function names> <list of parameters> <list of bodies>). 

	E.g.  
	(multifun (f g) (x)          ((+ x x x)           (* x x))) 

	defines the two functions f with body (+ x x x) and g with body (* x x), respectively.