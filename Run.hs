import DualNumbers

-- test cases
f1 :: Float -> Float -> Float
f1 x1 x2 = let first_term = log x1
	       second_term = x1 * x2
	       third_term = sin x2
	   in  first_term + second_term - third_term

f1' :: DualNumber -> DualNumber -> DualNumber
f1' x1 x2 = let first_term = ln' x1
		second_term = x1 * x2
		third_term = sin' x2 
	    in  first_term + second_term - third_term

