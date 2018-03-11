
str2action :: String -> IO ()
str2action input = putStrLn $ "Data: " ++ input

list2actions :: [String] -> [IO ()]
list2actions = map str2action

strings = show <$> [1..10]

actions :: [IO ()]
actions = list2actions strings

printall :: IO ()
printall = runall actions
  where runall [] = return ()
        runall (x:xs) = do
          putStr "awating: "
          x
          runall xs

main = do
  str2action "Start of the program"
  printall
  str2action "Done."
