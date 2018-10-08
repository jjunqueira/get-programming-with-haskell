-- first_prog.hs

toPart recipient = "Hello " ++ recipient ++ ",\n"

bodyPart bookTitle = "Thank you for buying " ++ bookTitle ++ ".\n"

fromPart author = "Cheers,\n" ++ author

createEmail recipient bookTitle author =
  toPart recipient ++ bodyPart bookTitle ++ fromPart author

messyMain :: IO ()
messyMain = do
  print "Who is this email for?"
  recipient <- getLine
  print "What is the title?"
  title <- getLine
  print "Who is the author?"
  author <- getLine
  print
    ("Dear " ++
     recipient ++
     ",\n" ++ "Thanks for buying " ++ title ++ "\nthanks,\n" ++ author)

main = do
  print "Who is the email for?"
  recipient <- getLine
  print "What is the title?"
  title <- getLine
  print "Who is the Author?"
  author <- getLine
  print (createEmail recipient title author)
