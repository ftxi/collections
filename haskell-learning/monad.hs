
-- the monad property of lists
-- similiar to the monad of Set -> Set

p :: Eq a => [a] -> [[a]]
p [] = [[]]
p (x:xs) = map (x:) (p xs) ++ p xs

t :: Eq a => a -> [a] -- unit
t x = [x]

u :: Eq a => [[a]] -> [a] -- join
u = unique . concat
  where unique [] = []
        unique (x:xs)
          | x `elem` xs = unique xs
          | otherwise   = x: unique xs

