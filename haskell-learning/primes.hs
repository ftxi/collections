module Main where

import IO

primes = sieve [2..]
  where sieve (x:xs) = x : sieve [u | u <- xs, u `mod` x /= 0]

main = do
  putStrLn "Tell me how many prime numbers do you want (0 for infinty)"
  str <- readLine
  let n = read str in
    if n > 0
    then print $ take n primes
    else print primes

