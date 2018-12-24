-- mergesort.hs

merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge u@(x:xs) v@(y:ys)
  | x > y  = y : merge u ys
  | x <= y = x : merge xs v 

mergeSort :: Ord a => [a] -> [a]
mergeSort us = let n = length us
               in if n < 2
                  then us
                  else let (xs, ys) = splitAt (n `div` 2) us
                       in merge (mergeSort xs) (mergeSort ys)
                          
