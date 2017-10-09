
prime = sieve [2..]
  where sieve x:xs
          = x : sieve [u | u<-xs, u `mod` x /= 0]

{-
main = do
  a' <- getLine
  b' <- getLine
  let a = read a' :: Integer
      b = read b' :: Integer
    in put -}
