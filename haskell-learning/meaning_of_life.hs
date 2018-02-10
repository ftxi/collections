
main = do
  str <- getLine
  let n = read str in
    if n /= 42
    then do
      putStrLn str
      main
    else putStr ""

