module Listas where

data Lista a = Empty | H a (Lista a) 

instance Eq a => Eq (Lista a) where
    (==) :: Lista a -> Lista a -> Bool
    Empty  == Empty  = True
    Empty  == H _ _  = False
    H _ _  == Empty  = False
    H x xs == H y ys = x == y && xs == ys
    
(<:>) :: a -> Lista a -> Lista a
(<:>) = H

(<++>) :: Lista a -> Lista a -> Lista a
Empty  <++> xs    = xs
H x xs <++> ys    = H x (xs <++> ys)
xs     <++> Empty = xs

instance Show a => Show (Lista a) where
    show :: Lista a -> String
    show xs = "[" ++ doShow xs

doShow :: (Show a) => Lista a -> String
doShow Empty  = "]"
doShow (H x xs) = show x ++ showComma xs ++ doShow xs

showComma :: (Show a) => Lista a -> String
showComma Empty = ""
showComma _     = ","

instance Functor Lista where
    fmap :: (a -> b) -> Lista a -> Lista b
    fmap f Empty    = Empty
    fmap f (H x xs) = H (f x) (fmap f xs)

instance Semigroup (Lista a) where
    (<>) :: Lista a -> Lista a -> Lista a
    (<>) = (<++>)

instance Monoid (Lista a) where
    mempty :: Lista a
    mempty = Empty

instance Applicative Lista where
    pure :: a -> Lista a
    pure x = H x Empty

    (<*>) :: Lista (a -> b) -> Lista a -> Lista b
    Empty  <*> _     = Empty
    _      <*> Empty = Empty 
    H f fs <*> xs    = fmap f xs <++> (fs <*> xs)

instance Monad Lista where
    return :: a -> Lista a
    return = pure

    (>>=) :: Lista a -> (a -> Lista b) -> Lista b
    Empty  >>= _ = Empty
    H x xs >>= f = f x <++> (xs >>= f)

x, y, z :: Lista Int
x = H 4 (H 2 (H 0 (H 1 (H 20 (H 3 (H 7 Empty))))))
y = H 20 (H 13 (H 42 (H 7 (H 24 (H 20 (H 6 (H 1 Empty)))))))
z = H 0 (H 2 (H (-7) Empty))
