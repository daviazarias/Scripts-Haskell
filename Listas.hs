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
infixr <:>

(<++>) :: Lista a -> Lista a -> Lista a
Empty  <++> xs = xs
H x xs <++> ys = H x (xs <++> ys)

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

instance Foldable Lista where
    foldMap :: Monoid m => (a -> m) -> Lista a -> m
    foldMap _ Empty    = mempty
    foldMap f (H x xs) = (f x) <> foldMap f xs

    foldr :: (a -> b -> b) -> b -> Lista a -> b
    foldr _ y Empty    = y
    foldr f y (H x xs) = f x (foldr f y xs)

    foldl :: (b -> a -> b) -> b -> Lista a -> b
    foldl _ y Empty    = y
    foldl f y (H x xs) = f (foldl f y xs) x

foldToList :: Foldable t => t a -> [a]
foldToList = foldMap (\x -> [x])

listToH :: [a] -> Lista a
listToH = foldMap (\x -> H x Empty)

x, y, z :: Lista Int
x = 4 <:> 2 <:> 0 <:> 1 <:> 20 <:> 3 <:> 7 <:> Empty
y = 20 <:> 13 <:> 42 <:> 7 <:> 24 <:> 20 <:> 6 <:> 1 <:> Empty
z = 0 <:> 2 <:> (-7) <:> Empty
