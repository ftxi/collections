{-
getInt :: IO Int
getInt = fmap read getLine

nextPalindrome :: Int -> Int
nextPalindrome x
  = let str    = show x
        halfst = take ((length str + 1) `div` 2) str 
        pal xs = xs ++ (drop (length str `mod` 2) . reverse) xs 
        num    = if reverse str < str
                 then halfst
                 else show (read halfst + 1)
    in read $ pal num
-}

inci :: [Char] -> [Char]
inci x = reverse $ rinci $ reverse x
  where rinci ('9':[]) = "01"
        rinci ('9':xs) = '0' : rinci xs
        rinci (x:xs)   = succ x : xs
        rinci [] = [] -- to mute a warning, this is trivial

nextp :: [Char] -> [Char]
nextp s = let len  = length s
              half = take ((len + 1) `div` 2) s
              pal t = t ++ drop (len `mod` 2) (reverse t)
              p0   = pal half 
          in if s < p0
             then p0
             else let p1 = pal (inci half) -- in case of something like '99'
                  in if length p1 > len + 1
                     then head p1 : drop 2 p1
                     else p1

main :: IO ()
main = do
  k <- fmap read getLine
  loop k
  where
    loop 0 = return ()
    loop k = do
      s <- getLine
      putStrLn $ nextp s
      loop (k - 1)
  
