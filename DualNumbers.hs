module DualNumbers
( DualNumber(..)
, sin'
, cos'
, sqrt'
, ln'
) where

-- types
data DualNumber = DualNumber { value      :: Double
			     , derivative :: Double
		  	     } deriving (Show, Ord, Eq)

-- elementary functions
-- tan, sec, csc, cot, power, exponential, exponetiated

instance Num DualNumber where
	x + y 	 = DualNumber (value x + value y) (derivative x + derivative y)
	x * y 	 = let new_val   = (value x * value y)
		       new_deriv = (value x) * (derivative y) + (value y) * (derivative x)
		   in DualNumber new_val new_deriv
        negate x = DualNumber (negate $ value x) (negate $ derivative x)
        
instance Fractional DualNumber where
	x / y = let new_val   = (value x / value y)
		    fst_term  = (value x) * (derivative y)
		    snd_term  = (derivative x) * (value y)
		    denom     = (value y) * (value y)
		    new_deriv = (fst_term - snd_term) / denom
		in DualNumber new_val new_deriv

sin' :: DualNumber -> DualNumber
sin' x = 
	let new_val   = sin $ value x
	    new_deriv = (derivative x) * (cos $ value x)
	in  DualNumber new_val new_deriv   

cos' :: DualNumber -> DualNumber
cos' x = 
	let new_val   = cos $ value x
	    new_deriv = (derivative x) * (negate $ sin $ value x)
	in DualNumber new_val new_deriv

sqrt' :: DualNumber -> DualNumber
sqrt' x = 
	let new_val   = sqrt $ value x
	    new_deriv = (derivative x) * (0.5 / new_val)
	in DualNumber new_val new_deriv

ln' :: DualNumber -> DualNumber
ln' x = 
	let new_val   = log $ value x
	    new_deriv = (derivative x) * (1 / (value x))
	in DualNumber new_val new_deriv


