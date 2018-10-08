--- hello.hs my first Haskell file!
main = do
  name <- getLine
  print ("Hello " ++ name ++ "!")
