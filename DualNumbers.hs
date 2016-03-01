
import Control.Applicative

data DualNum a = DualNum a a deriving (Show, Eq)
constD :: Num a => a -> DualNum a
constD x = DualNum x 0

idD :: Num a => a -> DualNum a
idD x = DualNum x 1

(><) :: Num a => (a -> a) -> (a -> a) -> (DualNum a -> DualNum a)
(f >< f') (DualNum a a') = DualNum (f a) (a' * f' a)

sqr :: Num a => a -> a
sqr x = x * x

instance Num a => Num (DualNum a) where
	fromInteger 				= constD . fromInteger
	DualNum x0 x' + DualNum y0 y'   	= DualNum (x0 + y0) (x' + y')
	x@(DualNum x0 x') * y@(DualNum y0 y')	= DualNum (x0 * y0) ((x' * y0) + (x0 * y'))
	negate 					= negate >< (negate . (1 * ))
	signum					= signum >< (0 * )

instance Fractional a => Fractional (DualNum a) where
	fromRational = constD . fromRational
	recip 	     = recip >< (negate . sqr . recip)

instance Floating a => Floating (DualNum a) where
	pi		= DualNum pi 0
	exp		= exp  >< exp
	log 	= log  >< recip
	sqrt 	= sqrt >< (recip . (2 * ) . sqrt)
	sin 	= sin  >< cos
	cos		= cos  >< (negate . sin)
	asin	= asin >< (recip . sqrt . (1 - ) . sqr)
	acos  	= acos >< (recip . negate . sqrt . (1 - ) . sqr)
	atan  	= atan >< (recip . (1 + ) . sqr)
	sinh 	= sinh >< cosh
	cosh 	= cosh >< sinh
	asinh 	= asinh >< (recip . sqrt . (1 + ) . sqr)
	acosh 	= acosh >< (recip . negate . sqrt . sqr . (-1 + ))
	atanh 	= atanh >< (recip . (1 - ) . sqr)

	
		       
		   
        
        

	
		    
		    
		    
		    
		
















	

