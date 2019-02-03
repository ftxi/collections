-- gaussian_integers.hs
-- to find prime numbers in Gaussian integers

import Data.Complex

-- merge two sorted sequence
-- f is a function that helps to compare
merge :: Ord b => (a -> b) -> [a] -> [a] -> [a]
merge _ xs [] = xs
merge _ [] ys = ys
merge f u@(x:xs) v@(y:ys)
  | f x > f y  = y : merge f u ys
  | f x <= f y = x : merge f xs v
merge _ _ _ = error "merge: this should not happen"

mergeSort :: Ord b => (a -> b) -> [a] -> [a]
mergeSort f us = let n = length us
               in if n < 2
                  then us
                  else let (xs, ys) = splitAt (n `div` 2) us
                       in merge f (mergeSort f xs) (mergeSort f ys)
               
-- ssq : "sum of squares"
ssq :: Num a => (a, a) -> a
ssq (a, b) = a*a + b*b

-- r'seq : "the integer points on the plane, sorted by distance from origin"
r'seq :: [(Integer, Integer)]
r'seq = loop 0 0 []
  where
    loop _ _ [] = loop 1 0 [(0,1), (1,1)]
    loop k l s@((x,y):_)
          | x < l     = let (us, vs) = splitAt (fromIntegral (l+1)) s
                        in us ++ loop k (l+1) vs
          | otherwise = let k' = k + 1
                            l' = floor $ sqrt $ fromIntegral k'
                            t  = zip (replicate (fromIntegral (k+2)) k') [0..k']
                            s' = merge ssq s $ t
                        in loop k' l' s'

-- multiplication of Complex (Integers), which is not provided
infix 7 `times`
times :: Num a => Complex a -> Complex a -> Complex a
(a:+b) `times` (c:+d) = (a*c-b*d) :+ (a*d+b*c)

-- return True if a+bi divides c+di
infix 4 `divides`
divides :: Integral a => Complex a -> Complex a -> Bool
(a:+b) `divides` (c:+d) = let (m:+n) = (a:+(-b)) `times` (c:+d)
                              p = ssq (a, b)
                          in (m `mod` p) == 0 && (n `mod` p) == 0

-- the Gaussian primes
g'primes :: [Complex Integer]
g'primes = sieve $ tail r'seq
  where sieve ((x,y):rest)
          = let ns = case () of _
                                  | y == 0 -> [x:+0,0:+x,(-x):+0,0:+(-x)]
                                  | x == y -> [x:+x,x:+(-x),(-x):+x,(-x):+(-x)]
                                  | otherwise ->  [x:+y,y:+x,(-y):+x,(-x):+y]
                                                 ++ [(-x):+(-y),(-y):+(-x)]
                                                 ++ [y:+(-x),x:+(-y)]
            in ns ++ sieve [(x,y)|(x,y) <- rest, not $ any (`divides` x:+y) ns]

main :: IO ()
--main = mapM_ (putStrLn . show) g'primes
main = print $ map (\(a:+b) -> (a,b)) g'primes
