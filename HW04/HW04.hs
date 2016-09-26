{-# OPTIONS_GHC -Wall #-}

module HW04 where
import Data.List ( replicate )

newtype Poly a = P [a]


indexing :: Num a => [a] -> [(a, Int)]
indexing l = (zip l [0..])



-- Exercise 1 -----------------------------------------

x :: Num a => Poly a
x = P [0, 1]

-- Exercise 2 ----------------------------------------

instance (Num a, Eq a) => Eq (Poly a) where
    (==) (P xs) (P ys)
        | xlen == ylen = xs == ys
        | xlen > ylen  = xs == bck_padding (xlen - ylen) ys
        | xlen < ylen  = bck_padding (ylen - xlen) xs == ys
      where
        xlen = length(xs)
        ylen = length(ys)
        bck_padding :: Num a => Int -> [a] -> [a]
        bck_padding n l = l ++ (replicate n 0)


-- Exercise 3 -----------------------------------------


instance (Num a, Eq a, Show a) => Show (Poly a) where
    show (P xs) =  concatP (reverse (helper (indexing xs))) where
        helper :: (Num a, Eq a, Show a) => [(a, Int)] -> [String]
        helper []         = []
        helper ((0,_):l) = helper l
        helper ((c,0):l) = (show c):(helper l)
        helper ((1,1):l) = "x":(helper l)
        helper ((1,n):l) = ("x^" ++ show n):(helper l)
        helper ((c,1):l) = (show c ++ "x"):(helper l)
        helper ((c,n):l) = (show c ++ "x^" ++ show n):(helper l)
        concatP :: [String] -> String
        concatP []     = ""
        concatP (y:[]) = y
        concatP (y:ys) = y ++ " + " ++ concatP ys

-- Exercise 4 -----------------------------------------

plus :: Num a => Poly a -> Poly a -> Poly a
plus (P xs) (P ys) = P (helper xs ys) where
    helper :: Num a => [a] -> [a] -> [a]
    helper l1 l2 = case (l1, l2) of
        ([], []) -> []
        (l, [])  -> l
        ([], l)  -> l
        ((x':xl), (y':yl)) -> (x' + y'):(helper xl yl)

-- Exercise 5 -----------------------------------------

instance Num a => Monoid (Poly a) where
    mempty = P []
    mappend = plus

times :: Num a => Poly a -> Poly a -> Poly a
times (P xs) (P ys) = mconcat (helper (indexing xs) ys) where
    frt_padding :: Num a => Int -> [a] -> [a]
    frt_padding n l = (replicate n 0) ++ l
    helper :: Num a => [(a, Int)] -> [a] -> [Poly a]
    helper [] _ = []
    helper ((c, n):xl) yl = (P (frt_padding n (map (*c) yl))):(helper xl yl)




-- Exercise 6 -----------------------------------------

instance Functor Poly where
    fmap f (P l) = P (map f l)

instance Num a => Num (Poly a) where
    (+) = plus
    (*) = times
    negate      = fmap ((*) (-1))
    fromInteger = (\y -> P [fromInteger y])
    -- No meaningful definitions exist
    abs    = undefined
    signum = undefined

-- Exercise 7 -----------------------------------------

applyP :: Num a => Poly a -> a -> a
applyP (P l) x' = foldr (+) 0 $ map (\(c, n) -> c * x'^n) (indexing l)

-- Exercise 8 -----------------------------------------

class Num a => Differentiable a where
    deriv  :: a -> a
    nderiv :: Int -> a -> a
    nderiv 0 a = a
    nderiv n a = nderiv (n-1) (deriv a)

-- Exercise 9 -----------------------------------------

instance Num a => Differentiable (Poly a) where
    deriv (P []) = P []
    deriv (P [y]) = P []
    deriv (P l) = P (tail (map (\(c, n) -> c * (fromIntegral n)) (indexing l)))
