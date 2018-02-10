
readInts :: IO [Int]
readInts = fmap (map read.words) getLine

primes = sieve [2..]
  where sieve (x:xs)
          = x : sieve [u | u<-xs, u `mod` x /= 0]

isprime p =
  if p < 37813
  then p `elem` take 4000 primes
  else no'divisor p primes
  where no'divisor p (x:xs)
          | p `mod` x == 0 = False
          | x > m          = True
          | otherwise      = no'divisor p xs
          where m = floor $ sqrt $ fromIntegral p

main = do
  t'str <- getLine
  loop (read t'str)
  where
    loop 0 = putStr ""
    loop k = do
      ints <- readInts
      let [m, n] = ints in do
        putStrLn $ concat $ map ((++"\n") . show) $ filter isprime [m..n]
        loop (k - 1)
        
