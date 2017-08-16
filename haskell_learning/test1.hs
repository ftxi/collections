
fact :: Integer -> Integer
fact n
    | n == 0 = 1
    | otherwise = n * fact (n-1)

minimum' :: (Ord a) => [a] -> a
minimum' [] = error "minimum in a empty list"
minimum' (x:[]) = x
-- minimum' (x:xs) = min x (minimum' xs)
minimum' (x:y:s) = if x < y
                    then minimum' (x:s)
                    else minimum' (y:s)

length' :: Num b => [a] -> b
length' [] = 0
length' (_:xs) = 1 + length' xs

qsort :: (Ord a) => [a] -> [a]
qsort [] = []
qsort (x:[]) = [x]
qsort (x:xs) = connecQ (partition x xs)
  where connecQ (a, k, b) = (qsort a) ++ [k] ++ (qsort b)
        partition x xs = ([u | u<-xs, u < x], x,
                          [v | v<-xs, v >= x])

