module Primitive where

data N = Zero | Succ N

toIntegral :: (Integral a) => N -> a
toIntegral Zero = 0
toIntegral (Succ n) = 1 + toIntegral n

fromIntegral :: (Integral a) => a -> Maybe N
fromIntegral i = if i < 0 then Nothing else Just . fromPos $ i 
    where
    fromPos x
        | x < 0 = undefined
        | x == 0 = Zero
        | otherwise = Succ $ fromPos (x - 1)

instance Show N where
    show = show . toIntegral

infixl 6 +:
(+:) :: N -> N -> N
(+:) x Zero = x
(+:) x (Succ y) = Succ (x +: y)

infixl 7 *:
(*:) :: N -> N -> N
(*:) x Zero = Zero
(*:) x (Succ y) = x *: y +: x

infixr 8 ^:
(^:) :: N -> N -> N
(^:) x Zero = Succ Zero
(^:) x (Succ y) = x ^: y *: x

pred :: N -> N
pred Zero = Zero
pred (Succ y) = y

infixl 6 -:
(-:) :: N -> N -> N
(-:) x Zero = x
(-:) x (Succ y) = Primitive.pred $ x -: y

factorial :: N -> N
factorial Zero = Succ Zero
factorial y@(Succ x) = y *: (factorial x)
