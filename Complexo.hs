data Complexo = Complexo Double Double

instance Eq Complexo where
    (==) :: Complexo -> Complexo -> Bool
    (Complexo a1 b1) == (Complexo a2 b2) = (a1 == a2) && (b1 == b2)

instance Show Complexo where
    show :: Complexo -> String
    show (Complexo a b) = show a ++ showSign ++ show (abs b) ++ "i"
        where showSign = if b >= 0 then " + " else " - "

instance Num Complexo where
    (+) :: Complexo -> Complexo -> Complexo
    (Complexo a1 b1) + (Complexo a2 b2) = Complexo (a1 + a2) (b1 + b2) 

    (*) :: Complexo -> Complexo -> Complexo
    (Complexo a1 b1) * (Complexo a2 b2) = Complexo (a1*a2 - b1*b2) (a1*b2 + a2*b1) 

    abs :: Complexo -> Complexo
    abs (Complexo a b) = Complexo (sqrt (a^2 + b^2)) 0

    negate :: Complexo -> Complexo
    negate (Complexo a b) = Complexo (-a) (-b)

    fromInteger :: Integer -> Complexo
    fromInteger n = Complexo (fromInteger n) 0

    signum :: Complexo -> Complexo 
    signum 0 = Complexo 0 0
    signum z = z / abs z

instance Fractional Complexo where
    (/) :: Complexo -> Complexo -> Complexo
    (Complexo a1 b1) / (Complexo a2 b2) = Complexo ((a1*a2 + b1*b2)/den) ((a2*b1 - a1*b2)/den)
        where den = a2^2 + b2^2

    fromRational :: Rational -> Complexo
    fromRational r = Complexo (fromRational r) 0 

instance Semigroup Complexo where
    (<>) :: Complexo -> Complexo -> Complexo
    (<>) = (+)

instance Monoid Complexo where
    mempty :: Complexo
    mempty = Complexo 0 0

conj :: Complexo -> Complexo
conj (Complexo a b) = Complexo a (-b)

real :: Complexo -> Double
real (Complexo a _) = a

imag :: Complexo -> Double
imag (Complexo _ b) = b