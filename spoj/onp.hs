
main = do
  k'str <- getLine
  loop $ read k'str
  where
    loop 0 = putStr ""
    loop k = do
      str <- getLine
      f str ""
      loop (k - 1)
      where
        f [] _ = putStrLn ""
        f (x:xs) buffer
          | x == '('  = f xs buffer
          | x `elem` "+-*/^"
                      = f xs (x:buffer)
          | x == ')'  = do putChar $ head buffer
                           f xs $ tail buffer
          | otherwise = do putChar x
                           f xs buffer
