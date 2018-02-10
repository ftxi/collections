
readInts :: IO [Int]
readInts = fmap (map read.words) getLine

primes = sieve [2..]
  where sieve (x:xs)
          = x : sieve [u | u<-xs, u `mod` x /= 0]

main = do
  t'str <- getLine
  loop (read t'str)
  where
    loop 0 = putStr ""
    loop k = do
      ints <- readInts
      let [m, n] = ints in do
        showbelow n $ cutbelow m primes
        loop (k - 1)
        where
          cutbelow m (x:xs) =
            if x < m
            then cutbelow m xs
            else x:xs
          showbelow n (x:xs) =
            if x > n
            then putStrLn ""
            else do
              print x
              showbelow n xs
