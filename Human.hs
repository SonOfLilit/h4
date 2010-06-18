module Human where
import H4(MoveChooser)

humanPlayer :: MoveChooser
humanPlayer board player = do     
  putStrLn "Board position:"
  print board
  putStrLn $ show player ++ ", choose a column:"
  line <- getLine
  return $ read line - 1
