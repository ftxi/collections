
{-

a'seq :: [(Integer, Integer)]
a'seq = (0,0):loop 0 1
  where loop x y
          | x < y     = let s1 = [(x,y),(y,x),(-y,x),(-x,y)]
                            s2 = [(-x,-y),(-y,-x),(y,-x),(x,-y)]
                        in s1 ++ s2 ++ loop (x+1) y
          | otherwise = (x,y):(-x,y):(-x,-y):(x,-y):loop 0 (y+1)

-}

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
ssq :: (Integer, Integer) -> Integer
ssq (a, b) = a*a + b*b

-- r'seq : "the integer points on the plane, sorted by distance from origin"
r'seq :: [(Integer, Integer)]
r'seq = loop 0 0 []
  where
    loop _ _ [] = loop 1 0 [(0,1), (1,1)]
    loop k l s@((x,y):_)
          | x < l = let (us, vs) = splitAt (fromIntegral (l+1)) s
                    in us ++ loop k (l+1) vs
          | otherwise = let k' = k + 1
                            l' = floor $ sqrt $ fromInteger k'
                            t  = zip (replicate (fromIntegral (k+2)) k') [0..k']
                            s' = merge ssq s $ t
                        in loop k' l' s'
                           
