
main :: IO ()
main =
  putStr "Input a: " >>
  getLine >>=
  (\a's ->
    putStr "Input b: " >>
    getLine >>=
    (\b's ->
       let num :: Double
           num =  read a's + read b's
       in putStrLn $ "the sum is: " ++ show num))
